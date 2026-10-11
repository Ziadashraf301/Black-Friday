"""
Cart mutation specialist node.
Executes stateful cart actions (add, remove, change size, clear, view subtotal).
"""

import re
from typing import Any, Dict, List, Optional, Tuple

from ai.schemas.identity import ASK_WHICH_PRODUCT, GUEST_USER_ID, unknown_product_message
from ai.schemas.state import AgentState
from ai.tools.cart_tools import cart_tool
from core.exceptions import DomainError
from core.logging import get_logger

logger = get_logger(__name__)


SIZE_MUTATION_RE = re.compile(r"\bfrom\s+(?:size\s+)?([a-z0-9]+)\s+to\s+(?:size\s+)?([a-z0-9]+)\b")
SIZE_CHANGE_RE = re.compile(r"\b(?:change|switch|swap)\b.*\bsize\b")
CART_ACTION_RES = (
    ("add", re.compile(r"\b(?:add|put|buy|place)\b")),
    ("remove", re.compile(r"\b(?:remove|delete|drop)\b|\btake\s+out\b")),
    ("clear", re.compile(r"\b(?:clear|empty)\b")),
)


def detect_cart_action(query: str, sizes: List[str]) -> Tuple[str, Optional[str], Optional[str]]:
    """Returns (action, old_size, new_size) for a lower-cased cart query; words are matched whole, not as substrings."""
    mutation_m = SIZE_MUTATION_RE.search(query)
    if mutation_m:
        return "update_size", mutation_m.group(1).upper(), mutation_m.group(2).upper()
    if SIZE_CHANGE_RE.search(query) and len(sizes) >= 2:
        return "update_size", sizes[0], sizes[1]
    for action, pattern in CART_ACTION_RES:
        if pattern.search(query):
            return action, None, None
    return "get", None, None


def cart_agent_node(state: AgentState) -> Dict[str, Any]:
    """
    Executes stateful cart mutations (add, remove, change size from L to XL, subtotal).
    """
    query = state.get("query", "").lower()
    user_id = state.get("user_id", GUEST_USER_ID)
    session_id = state.get("session_id", "default_session")
    entities = state.get("entities") or {}
    product_ids = entities.get("product_ids") or []
    sizes = entities.get("sizes") or []

    target_pid = product_ids[0] if product_ids else None
    action, old_size, new_size = detect_cart_action(query, sizes)

    target_size = new_size or (sizes[0] if sizes else None)

    logger.info(f"[GRAPH: CART-NODE] Executing cart action={action}, pid={target_pid}, size={target_size}")

    if action == "add" and not target_pid:
        logger.info("[GRAPH: CART-NODE] Add requested without a resolvable product; asking the shopper")
        return {"clarification": ASK_WHICH_PRODUCT, "current_node": "cart_agent_node"}

    try:
        updated_cart = cart_tool.modify_cart(
            user_id=user_id,
            session_id=session_id,
            action=action,
            product_id=target_pid,
            size=old_size if action == "update_size" else target_size,
            new_size=new_size if action == "update_size" else None,
            quantity=1,
        )
    except DomainError as e:
        logger.info(f"[GRAPH: CART-NODE] Cart change rejected: {e}")
        return {"clarification": unknown_product_message(target_pid or "that item"), "current_node": "cart_agent_node"}

    return {
        "cart": updated_cart.model_dump(),
        "ui_payload": {
            "action_chips": ["View Cart", "Checkout Now"],
        },
        "current_node": "cart_agent_node",
    }
