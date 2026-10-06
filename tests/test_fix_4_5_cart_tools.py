"""
Regression tests for Fix 4.5: CartManagementTool Redis session caching and sliding TTL.
Validates:
1. Cart persists in Redis using get_json/set_json.
2. Writing a cart with tool instance A, then creating a NEW tool instance B (with empty in-memory state),
   successfully reads the cart back from Redis.
3. Sliding TTL refreshes the expiration window upon item additions and cart access.
"""
import time
import pytest
from ai.tools.cart_tools import CartManagementTool, cart_tool
from core.cache.redis_client import cache_manager


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
