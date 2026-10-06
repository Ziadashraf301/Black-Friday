"""
Unit and Integration Tests for Cold-Tier Cart Durability & Stock Validation (Phase 4 - Task P4-06).
Validates:
  1. Real-time catalog stock validation.
  2. Dual-tier saving: Redis 6-hour sliding session and PostgreSQL user_carts snapshot.
  3. Seamless cart hydration from PostgreSQL cold-tier upon Redis cache eviction.
"""
import pytest
from ai.tools.cart_tools import cart_tool
from core.db.repository import BlackFridayRepository
from core.cache.redis_client import RedisCacheManager


class TestCartDurabilityAndStock:
    """Verifies stock checks, dual-tier cart persistence, and cold hydration from PostgreSQL."""

    @pytest.fixture(autouse=True)
    def setup_env(self):
        self.repo = BlackFridayRepository()
        self.cache = RedisCacheManager()
        self.user_id = "test_user_durability_99"
        self.session_id = "sess_durability_99"

    def test_stock_validation(self):
        # Product P00025442 exists with sizes ['S', 'M', 'L', 'XL']
        assert self.repo.check_product_stock("P00025442", "M") is True
        assert self.repo.check_product_stock("P00025442", "S") is True
        # Size that does NOT exist
        assert self.repo.check_product_stock("P00025442", "XXXXL") is False

    def test_dual_tier_save_and_cold_hydration(self):
        # 1. Clear cart
        cart_tool.modify_cart(self.user_id, self.session_id, action="clear")

        # 2. Add item to cart
        cart_state = cart_tool.modify_cart(
            user_id=self.user_id,
            session_id=self.session_id,
            action="add",
            product_id="P00025442",
            size="M",
            quantity=2,
        )
        assert cart_state.item_count == 2
        assert len(cart_state.items) == 1
        assert cart_state.items[0].product_id == "P00025442"

        # Verify cold snapshot exists in PostgreSQL
        cold_data = self.repo.load_user_cart_snapshot(self.user_id)
        assert cold_data is not None
        assert cold_data["item_count"] == 2
        assert len(cold_data["items"]) == 1

        # 3. Simulate complete Redis cache eviction/loss
        cart_key = cart_tool._get_cart_key(self.user_id, self.session_id)
        if self.cache.is_available:
            self.cache.delete_key(cart_key)
        # Clear in-memory cart map as well
        cart_tool._in_memory_carts.pop(cart_key, None)

        # 4. Fetch cart: must hydrate from PostgreSQL cold-tier snapshot!
        hydrated_cart = cart_tool.get_cart(self.user_id, self.session_id)
        assert hydrated_cart is not None
        assert hydrated_cart.item_count == 2
        assert len(hydrated_cart.items) == 1
        assert hydrated_cart.items[0].product_id == "P00025442"
        assert hydrated_cart.items[0].quantity == 2

        # 5. Clean up
        cart_tool.modify_cart(self.user_id, self.session_id, action="clear")
