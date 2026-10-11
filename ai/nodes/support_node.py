"""
Order support & policy specialist node.
Handles delivery tracking inquiries and Black Friday policy details.
"""

import re
from typing import Any, Dict

from ai.schemas.identity import GUEST_USER_ID
from ai.schemas.state import AgentState
from ai.tools.order_tools import order_tool
from core.logging import get_logger

logger = get_logger(__name__)

ORDER_ID_RE = re.compile(r"\b(ORD-\d{4,8})\b", re.I)


def support_agent_node(state: AgentState) -> Dict[str, Any]:
    """
    Handles delivery tracking inquiries and Black Friday policy details.
    """
    query = state.get("query", "")
    q = query.lower()
    user_id = state.get("user_id", GUEST_USER_ID)

    # Check for order code
    order_m = ORDER_ID_RE.search(query)
    order_status_payload = None
    policy_payload = None

    if order_m or "track" in q or "where is my" in q:
        order_id = order_m.group(1).upper() if order_m else None
        logger.info(f"[GRAPH: SUPPORT-NODE] Tracking order={order_id or 'latest'}")
        order_status = order_tool.track_order(order_id=order_id, user_id=user_id)
        order_status_payload = order_status.model_dump()
    else:
        # Determine topic from query keywords
        topic = "returns"
        if any(w in q for w in ["ship", "deliver", "arrive"]):
            topic = "shipping"
        elif any(w in q for w in ["price match", "refund difference", "price drop"]):
            topic = "price_match"
        elif any(w in q for w in ["discount", "coupon", "code", "promo"]):
            topic = "discounts"

        logger.info(f"[GRAPH: SUPPORT-NODE] Fetching policy details for topic={topic}")
        policy_payload = order_tool.get_policy(topic=topic)

    return {
        "order_status": order_status_payload,
        "policy_details": policy_payload,
        "ui_payload": {
            "action_chips": ["Contact Support", "Store Policies"],
        },
        "current_node": "support_agent_node",
    }
