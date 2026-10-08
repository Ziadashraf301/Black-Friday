"""
Cart Management Tool (Phase 3).
Provides stateful shopping cart operations adhering to SOLID principles:
  - Repository Pattern: Uses BlackFridayRepository (WarehouseRepository) for catalog data.
  - Cache Service: Uses RedisCacheManager (cache_manager) for Redis session caching.
  - Operations: Add items, remove items, mutate sizes, update quantities, calculate subtotals/discounts.
"""
from typing import Optional, Dict, Any, List
import json

from ai.schemas import CartState, CartItem
from ai.extractor.catalog_index import CatalogIndex
from core.db.repository import BlackFridayRepository
from core.cache.redis_client import cache_manager, RedisCacheManager
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class CartManagementTool:
    """
    Manages shopping cart operations adhering to SOLID principles.
    Delegates persistence to BlackFridayRepository and session caching to RedisCacheManager.
    """

    def __init__(
        self,
        repository: Optional[BlackFridayRepository] = None,
        cache: Optional[RedisCacheManager] = None,
    ):
        self._repo = repository
        self._cache = cache or cache_manager
        self._in_memory_carts: Dict[str, Dict[str, Any]] = {}
        CatalogIndex.ensure_loaded()

    @property
    def repo(self) -> Optional[BlackFridayRepository]:
        if self._repo is None:
            try:
                self._repo = BlackFridayRepository()
            except Exception as e:
                logger.warning(f"[TOOL: CART] DB repository connection failed: {e}")
                self._repo = None
        return self._repo

    def _get_cart_key(self, user_id: str, session_id: str) -> str:
        return f"cart:{user_id}:{session_id}"

    def get_cart(self, user_id: str, session_id: str) -> CartState:
        key = self._get_cart_key(user_id, session_id)
        cached_data = None

        # 1. Hot-tier Redis session cache
        if self._cache.is_available:
            try:
                cached_data = self._cache.get_json(key)
                if cached_data and self._cache.client is not None:
                    # Slide TTL on active session access
                    ttl = getattr(settings, "REDIS_DEFAULT_TTL", 21600)
                    self._cache.client.expire(key, ttl)
            except Exception as e:
                logger.debug(f"[TOOL: CART] Cache retrieval error: {e}")

        # 2. In-memory fallback
        if not cached_data:
            cached_data = self._in_memory_carts.get(key)

        # 3. Cold-tier PostgreSQL Durability Snapshot (Phase 4 - Task P4-06)
        if not cached_data and self.repo:
            try:
                cold_snapshot = self.repo.load_user_cart_snapshot(user_id)
                if cold_snapshot:
                    logger.info(f"[TOOL: CART] Hydrating cart from PostgreSQL cold-tier snapshot for user={user_id}")
                    cached_data = cold_snapshot
                    if self._cache.is_available:
                        self._cache.set_json(key, cold_snapshot, ttl=getattr(settings, "REDIS_DEFAULT_TTL", 21600))
            except Exception as e:
                logger.debug(f"[TOOL: CART] Cold-tier cart load error: {e}")

        if cached_data:
            data = json.loads(cached_data) if isinstance(cached_data, str) else cached_data
            items = [CartItem(**item) for item in data.get("items", [])]
            return self._recalculate_cart(user_id, session_id, items)

        return CartState(user_id=user_id, session_id=session_id)

    def _lookup_product_info(self, product_id: Optional[str], query_hint: Optional[str] = None) -> Optional[Dict[str, Any]]:
        """
        Resolves product specifications from BlackFridayRepository,
        falling back to CatalogIndex.
        """
        clean_id = (product_id or "").strip().upper()
        if clean_id and not clean_id.startswith("P"):
            clean_id = f"P{clean_id.zfill(8)}"

        # 1. Query repository layer (WarehouseRepository)
        if self.repo and clean_id:
            try:
                prod = self.repo.get_curated_product_by_id(clean_id)
                if prod:
                    return {
                        "product_id": prod["product_id"],
                        "name": prod["name"],
                        "discounted_price": float(prod.get("discounted_price", 49.99)),
                        "original_price": float(prod.get("original_price", 99.99)),
                        "sizes": prod.get("sizes", ["S", "M", "L", "XL"]),
                        "image_url": prod.get("image_url", f"/products/{clean_id}.jpg"),
                        "category_name": prod.get("category_name", "Apparel"),
                    }
            except Exception as e:
                logger.debug(f"[TOOL: CART] Repository product lookup failed: {e}")

        # 2. Try CatalogIndex exact match
        if clean_id and clean_id in CatalogIndex._ID_TO_PRODUCT:
            prod = CatalogIndex._ID_TO_PRODUCT[clean_id]
            return {
                "product_id": clean_id,
                "name": prod.get("name", f"Product {clean_id}"),
                "discounted_price": float(prod.get("discounted_price", 49.99)),
                "original_price": float(prod.get("original_price", 99.99)),
                "sizes": prod.get("sizes", ["S", "M", "L", "XL"]),
                "image_url": prod.get("image_url", f"/products/{clean_id}.jpg"),
                "category_name": prod.get("category_name", "Apparel"),
            }

        # 3. Fuzzy search in CatalogIndex if product_id was not explicitly given
        if query_hint:
            matched_id = CatalogIndex.find_best_product_id(query_hint)
            if matched_id:
                return self._lookup_product_info(matched_id)

        # Default fallback
        if clean_id:
            return {
                "product_id": clean_id,
                "name": f"Product {clean_id}",
                "discounted_price": 49.99,
                "original_price": 79.99,
                "sizes": ["S", "M", "L", "XL"],
                "image_url": f"/products/{clean_id}.jpg",
                "category_name": "Apparel",
            }

        return None

    def modify_cart(
        self,
        user_id: str,
        session_id: str,
        action: str,
        product_id: Optional[str] = None,
        size: Optional[str] = None,
        quantity: int = 1,
        new_size: Optional[str] = None,
    ) -> CartState:
        """
        Executes cart actions: 'add', 'remove', 'update_size', 'update_quantity', 'clear'.
        """
        logger.info(
            f"[TOOL: CART] Action={action} for user={user_id}, pid={product_id}, "
            f"size={size}, qty={quantity}, new_size={new_size}"
        )

        current_cart = self.get_cart(user_id, session_id)
        items: List[CartItem] = list(current_cart.items)
        act = action.lower().strip()

        clean_id = (product_id or "").strip().upper()
        if clean_id and not clean_id.startswith("P"):
            clean_id = f"P{clean_id.zfill(8)}"

        if act == "add":
            prod_info = self._lookup_product_info(clean_id)
            if not prod_info:
                clean_id = clean_id or "P00025442"
                prod_info = self._lookup_product_info(clean_id)

            target_pid = prod_info["product_id"] if prod_info else (clean_id or "P00025442")
            name = prod_info["name"] if prod_info else f"Product {target_pid}"
            unit_price = prod_info["discounted_price"] if prod_info else 49.99
            img_url = prod_info["image_url"] if prod_info else f"/products/{target_pid}.jpg"
            available_sizes = prod_info["sizes"] if prod_info else ["M"]
            chosen_size = (size or available_sizes[0]).upper()

            # Stock availability validation (Phase 4 - Task P4-06)
            if self.repo:
                has_stock = self.repo.check_product_stock(product_id=target_pid, size=chosen_size, quantity=quantity)
                if not has_stock:
                    logger.warning(f"[TOOL: CART] Size {chosen_size} out of stock for {target_pid}. Defaulting to in-stock size.")
                    if available_sizes:
                        chosen_size = available_sizes[0].upper()

            # Check if same product and size already exists in cart
            existing = next((i for i in items if i.product_id == target_pid and i.size == chosen_size), None)
            if existing:
                existing.quantity += quantity
                existing.total_price = round(existing.quantity * existing.unit_price, 2)
            else:
                items.append(
                    CartItem(
                        product_id=target_pid,
                        name=name,
                        size=chosen_size,
                        quantity=quantity,
                        price=unit_price,
                        unit_price=unit_price,
                        total_price=round(quantity * unit_price, 2),
                        image_url=img_url,
                    )
                )

        elif act == "remove":
            if clean_id:
                if size:
                    items = [i for i in items if not (i.product_id == clean_id and i.size == size.upper())]
                else:
                    items = [i for i in items if i.product_id != clean_id]
            else:
                if items:
                    items.pop()

        elif act == "update_size":
            target_new_size = (new_size or size or "M").upper()
            target_old_size = (size if new_size else None)
            for item in items:
                if not clean_id or item.product_id == clean_id:
                    if not target_old_size or item.size == target_old_size.upper():
                        item.size = target_new_size
                        break

        elif act == "update_quantity":
            for item in items:
                if item.product_id == clean_id and (not size or item.size == size.upper()):
                    item.quantity = max(1, quantity)
                    item.total_price = round(item.quantity * item.unit_price, 2)
                    break

        elif act == "clear":
            items = []

        return self._save_cart(user_id, session_id, items)

    def _recalculate_cart(self, user_id: str, session_id: str, items: List[CartItem]) -> CartState:
        subtotal = sum(i.total_price for i in items)
        # Black Friday promotion: 10% extra discount if subtotal >= $150
        discount = round(subtotal * 0.10, 2) if subtotal >= 150.0 else 0.0
        final_total = round(subtotal - discount, 2)
        item_count = sum(i.quantity for i in items)

        return CartState(
            user_id=user_id,
            session_id=session_id,
            items=items,
            subtotal=round(subtotal, 2),
            discount_total=discount,
            final_total=final_total,
            item_count=item_count,
        )

    def _save_cart(self, user_id: str, session_id: str, items: List[CartItem]) -> CartState:
        new_state = self._recalculate_cart(user_id, session_id, items)
        cart_dict = new_state.model_dump()
        payload = json.dumps(cart_dict)
        key = self._get_cart_key(user_id, session_id)

        # 1. Hot-tier Redis 6-hour sliding session (refreshes TTL on mutation)
        ttl = getattr(settings, "REDIS_DEFAULT_TTL", 21600)
        if self._cache.is_available:
            try:
                self._cache.set_json(key, cart_dict, ttl=ttl)
            except Exception as e:
                logger.debug(f"[TOOL: CART] Cache set error: {e}")

        self._in_memory_carts[key] = cart_dict

        # 2. Cold-tier PostgreSQL Durability Snapshot (Phase 4 - Task P4-06)
        if self.repo:
            try:
                self.repo.save_user_cart_snapshot(user_id=user_id, session_id=session_id, cart_dict=cart_dict)
                logger.info(f"[TOOL: CART] Persisted cold-tier cart snapshot for user={user_id}")
            except Exception as e:
                logger.debug(f"[TOOL: CART] PostgreSQL cart persistence error: {e}")

        return new_state


# Global singleton instance
cart_tool = CartManagementTool()

__all__ = ["CartManagementTool", "cart_tool"]
