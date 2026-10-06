"""
Cart mutation specialist node.
Executes stateful cart actions (add, remove, change size, clear, view subtotal).
"""
import re
from typing import Dict, Any
from loguru import logger

from ai.workflow.state import AgentState
from ai.tools.cart_tools import cart_tool


def cart_agent_node(state: AgentState) -> Dict[str, Any]:
    """
    Executes stateful cart mutations (add, remove, change size from L to XL, subtotal).
    """
    query = state.get("query", "").lower()
    user_id = state.get("user_id", "guest_user")
    session_id = state.get("session_id", "default_session")
    entities = state.get("entities") or {}
    product_ids = entities.get("product_ids") or []
    sizes = entities.get("sizes") or []

    target_pid = product_ids[0] if product_ids else None

    # Detect action
    action = "get"
    new_size = None
    old_size = None

    if "from" in query and "to" in query:
        action = "update_size"
        mutation_m = re.search(r"\bfrom\s+(?:size\s+)?([a-z0-9]+)\s+to\s+(?:size\s+)?([a-z0-9]+)\b", query)
        if mutation_m:
            old_size = mutation_m.group(1).upper()
            new_size = mutation_m.group(2).upper()
        elif len(sizes) >= 2:
            old_size = sizes[0]
            new_size = sizes[1]
    elif any(w in query for w in ["add", "put", "buy", "place"]):
        action = "add"
    elif any(w in query for w in ["remove", "delete", "drop", "take out"]):
        action = "remove"
    elif any(w in query for w in ["clear", "empty"]):
        action = "clear"

    target_size = new_size or (sizes[0] if sizes else None)

    logger.info(f"[GRAPH: CART-NODE] Executing cart action={action}, pid={target_pid}, size={target_size}")

    updated_cart = cart_tool.modify_cart(
        user_id=user_id,
        session_id=session_id,
        action=action,
        product_id=target_pid,
        size=old_size if action == "update_size" else target_size,
        new_size=new_size if action == "update_size" else None,
        quantity=1,
    )

    return {
        "cart": updated_cart.model_dump(),
        "current_node": "cart_agent_node",
    }
