"""
Product Details Specialist Node (Phase 3).
Fetches comprehensive specifications, materials, and care instructions for catalog products.
"""

import re
from typing import Any, Dict

from ai.schemas.identity import ASK_WHICH_PRODUCT, unknown_product_message
from ai.schemas.state import AgentState
from ai.tools.catalog_tools import catalog_details_tool
from core.logging import get_logger

logger = get_logger(__name__)

PRODUCT_ID_RE = re.compile(r"\b[pP][\s\-_#]?([0-9oO]{4,8})\b")


def details_agent_node(state: AgentState) -> Dict[str, Any]:
    """
    Fetches comprehensive product specifications, materials, and care instructions.
    """
    query = state.get("query", "")
    entities = state.get("entities") or {}
    product_ids = entities.get("product_ids") or []

    # If product ID not in entities, scan query directly
    if not product_ids:
        p_match = PRODUCT_ID_RE.search(query)
        if p_match:
            raw_digits = p_match.group(1).upper().replace("O", "0")
            product_ids = [f"P{raw_digits.zfill(8)}"]

    if not product_ids:
        logger.info("[GRAPH: DETAILS-NODE] No product resolved from the query; asking the shopper")
        return {"clarification": ASK_WHICH_PRODUCT, "current_node": "details_agent_node"}

    target_pid = product_ids[0]
    logger.info(f"[GRAPH: DETAILS-NODE] Fetching details for product_id={target_pid}")

    details = catalog_details_tool.get_details(target_pid)
    details_payload = details.model_dump() if details else None
    if details_payload is None:
        return {"clarification": unknown_product_message(target_pid), "current_node": "details_agent_node"}

    cards = []
    if details_payload and details_payload.get("product_id"):
        cards.append(
            {
                "type": "PRODUCT_DETAIL_CARD",
                "product_id": details_payload.get("product_id"),
                "name": details_payload.get("name"),
                "price": details_payload.get("discounted_price", details_payload.get("price")),
                "original_price": details_payload.get("original_price"),
                "image_url": details_payload.get("image_url", f"/products/{details_payload.get('product_id')}.jpg"),
                "materials": details_payload.get("materials", []),
                "care_instructions": details_payload.get("care_instructions"),
                "sizes": details_payload.get("sizes", []),
                "stock_status": details_payload.get("stock_status", "IN_STOCK"),
            }
        )

    return {
        "product_details": details_payload,
        "ui_payload": {
            "cards": cards,
            "action_chips": ["Select Size", "Add to Cart"],
        },
        "current_node": "details_agent_node",
    }


__all__ = ["details_agent_node"]
