"""
Bundle recommendations specialist node.
Retrieves statistical Apriori cross-sell associations and Item2Vec bundles.
"""
from typing import Dict, Any
from loguru import logger

from ai.workflow.state import AgentState
from ai.tools.bundle_tools import bundle_tool


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

    return {
        "bundle_recommendations": bundles_payload,
        "current_node": "bundle_agent_node",
    }
