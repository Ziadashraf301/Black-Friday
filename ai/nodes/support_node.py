"""
Order support & policy specialist node.
Handles delivery tracking inquiries and Black Friday policy details.
"""
import re
from typing import Dict, Any
from loguru import logger

from ai.workflow.state import AgentState
from ai.tools.order_tools import order_tool


def support_agent_node(state: AgentState) -> Dict[str, Any]:
    """
    Handles delivery tracking inquiries and Black Friday policy details.
    """
    query = state.get("query", "")
    user_id = state.get("user_id", "guest_user")

    # Check for order code
    order_m = re.search(r"\b(ORD-\d{4,8})\b", query, re.I)
    order_status_payload = None
    policy_payload = None

    if order_m or "track" in query.lower() or "where is my" in query.lower():
        order_id = order_m.group(1).upper() if order_m else "ORD-9842"
        logger.info(f"[GRAPH: SUPPORT-NODE] Tracking order={order_id}")
        order_status = order_tool.track_order(order_id=order_id, user_id=user_id)
        order_status_payload = order_status.model_dump()
    else:
        # Determine topic from query keywords
        topic = "returns"
        if any(w in query.lower() for w in ["ship", "deliver", "arrive"]):
            topic = "shipping"
        elif any(w in query.lower() for w in ["price match", "refund difference", "price drop"]):
            topic = "price_match"
        elif any(w in query.lower() for w in ["discount", "coupon", "code", "promo"]):
            topic = "discounts"

        logger.info(f"[GRAPH: SUPPORT-NODE] Fetching policy details for topic={topic}")
        policy_payload = order_tool.get_policy(topic=topic)

    return {
        "order_status": order_status_payload,
        "policy_details": policy_payload,
        "current_node": "support_agent_node",
    }

