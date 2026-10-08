"""
Bundle recommendations specialist node.
Retrieves statistical Apriori cross-sell associations and Item2Vec bundles.
"""
from typing import Dict, Any
from core.logging import get_logger

from ai.workflow.state import AgentState
from ai.tools.bundle_tools import bundle_tool

logger = get_logger(__name__)


def bundle_agent_node(state: AgentState) -> Dict[str, Any]:
    """
    Retrieves statistical Apriori cross-sell associations and Item2Vec bundles.
    """
    entities = state.get("entities") or {}
    product_ids = entities.get("product_ids") or []
    target_pid = product_ids[0] if product_ids else "P00025442"

    logger.info(f"[GRAPH: BUNDLE-NODE] Fetching bundles for product_id={target_pid}")

    bundles = bundle_tool.get_recommendations(target_pid, bundle_type="all")
    bundles_payload = [b.model_dump() for b in bundles]

    cards = [
        {
            "type": "BUNDLE_CARD",
            "product_id": b.get("product_id"),
            "name": b.get("name"),
            "price": b.get("price"),
            "bundle_price": b.get("bundle_price"),
            "discount_pct": b.get("discount_pct", "15% OFF"),
            "image_url": b.get("image_url", f"/products/{b.get('product_id')}.jpg"),
        }
        for b in bundles_payload
        if b.get("product_id")
    ]

    return {
        "bundle_recommendations": bundles_payload,
        "ui_payload": {
            "cards": cards,
            "action_chips": ["Add Bundle to Cart"],
        },
        "current_node": "bundle_agent_node",
    }
