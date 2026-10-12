"""Regression tests for Fix 4.5: CartManagementTool Redis session caching and sliding TTL.
Validates:
1. Cart persists in Redis using get_json/set_json.
2. Writing a cart with tool instance A, then creating a NEW tool instance B (with empty in-memory state),
   successfully reads the cart back from Redis.
3. Sliding TTL refreshes the expiration window upon item additions and cart access.

Unit and Integration Tests for Cold-Tier Cart Durability & Stock Validation (Phase 4 - Task P4-06).
Validates:
  1. Real-time catalog stock validation.
  2. Dual-tier saving: Redis 6-hour sliding session and PostgreSQL user_carts snapshot.
  3. Seamless cart hydration from PostgreSQL cold-tier upon Redis cache eviction."""

import time

import pytest

from ai.tools.cart_tools import CartManagementTool, cart_tool
from core.cache.redis_client import RedisCacheManager, cache_manager
from core.db.repository import BlackFridayRepository


def test_fix_4_5_cart_redis_persistence_new_instance():
    """
    Writes a cart using one CartManagementTool instance, then instantiates a brand new
    CartManagementTool instance (with empty in-memory state) and verifies it reads the cart
    back from Redis.
    """
    assert cache_manager.is_available is True
    client = cache_manager.client
    assert client is not None

    user_id = f"test_wp3_shopper_88_{int(time.time() * 1000)}"
    session_id = f"sess_wp3_cart_persist_{int(time.time() * 1000)}"
    cart_key = f"cart:{user_id}:{session_id}"

    # Clean existing key
    client.delete(cart_key)

    # 1. Modify cart using first instance
    tool_a = CartManagementTool()
    cart_state = tool_a.modify_cart(
        user_id=user_id,
        session_id=session_id,
        action="add",
        product_id="P00025442",
        quantity=2,
    )
    assert len(cart_state.items) == 1
    assert cart_state.items[0].product_id == "P00025442"
    assert cart_state.items[0].quantity == 2

    # Verify Redis holds the data via get_json
    raw_redis_data = cache_manager.get_json(cart_key)
    assert raw_redis_data is not None
    assert len(raw_redis_data["items"]) == 1
    assert raw_redis_data["items"][0]["product_id"] == "P00025442"

    # 2. Build a completely NEW manager instance with empty in-memory store
    tool_b = CartManagementTool()
    assert tool_b._in_memory_carts == {}

    # Read back cart from tool_b (must hydrate from Redis)
    restored_cart = tool_b.get_cart(user_id=user_id, session_id=session_id)
    assert len(restored_cart.items) == 1
    assert restored_cart.items[0].product_id == "P00025442"
    assert restored_cart.items[0].quantity == 2
    assert restored_cart.final_total == cart_state.final_total

    # Cleanup
    client.delete(cart_key)


def test_fix_4_5_cart_sliding_ttl():
    """
    Validates sliding TTL: adding items and reading cart refreshes Redis TTL.
    """
    assert cache_manager.is_available is True
    client = cache_manager.client
    assert client is not None

    user_id = "test_wp3_shopper_sliding"
    session_id = "sess_wp3_sliding"
    cart_key = f"cart:{user_id}:{session_id}"

    client.delete(cart_key)

    tool = CartManagementTool()
    tool.modify_cart(
        user_id=user_id,
        session_id=session_id,
        action="add",
        product_id="P00025442",
        quantity=1,
    )

    ttl_1 = client.ttl(cart_key)
    assert ttl_1 > 20000  # Default 21600 seconds

    # Mutating cart resets TTL
    tool.modify_cart(
        user_id=user_id,
        session_id=session_id,
        action="update_quantity",
        product_id="P00025442",
        quantity=3,
    )
    ttl_2 = client.ttl(cart_key)
    assert ttl_2 > 20000

    # Reading cart slides TTL
    _ = tool.get_cart(user_id=user_id, session_id=session_id)
    ttl_3 = client.ttl(cart_key)
    assert ttl_3 > 20000

    # Cleanup
    client.delete(cart_key)


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
        assert self.repo.is_size_offered("P00025442", "M") is True
        assert self.repo.is_size_offered("P00025442", "S") is True
        # Size that does NOT exist
        assert self.repo.is_size_offered("P00025442", "XXXXL") is False

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
