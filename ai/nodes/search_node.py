"""
Search Specialist Node (Phase 3).
Executes hybrid pgvector HNSW + tsvector BM25 search with Reciprocal Rank Fusion.
"""
from typing import Dict, Any
from ai.workflow.state import AgentState
from ai.tools.search_tools import search_tool
from core.logging import get_logger

logger = get_logger(__name__)


def search_agent_node(state: AgentState) -> Dict[str, Any]:
    """
    Executes hybrid product discovery using pre-extracted entities and query.
    """
    query = state.get("query", "")
    entities = state.get("entities") or {}

    max_price = entities.get("max_price")
    sizes = entities.get("sizes") or []
    target_size = sizes[0] if sizes else None
    categories = entities.get("categories") or []
    target_cat = categories[0] if categories else None

    logger.info(f"[GRAPH: SEARCH-NODE] Searching with cat={target_cat}, max_price={max_price}, size={target_size}")

    results = search_tool.search(
        query=query,
        category=target_cat,
        max_price=max_price,
        size=target_size,
        top_k=4,
    )

    products_payload = [r.model_dump() for r in results]
    relaxation_level = results[0].relaxation_level if results else "TIER_1_STRICT"

    cards = [
        {
            "type": "PRODUCT_CARD",
            "product_id": p.get("product_id"),
            "name": p.get("name"),
            "price": p.get("discounted_price", p.get("price")),
            "original_price": p.get("original_price"),
            "image_url": p.get("image_url", f"/products/{p.get('product_id')}.jpg"),
            "badge": p.get("badge", "Catalog Item"),
        }
        for p in products_payload
        if p.get("product_id")
    ]

    return {
        "retrieved_products": products_payload,
        "relaxation_level": relaxation_level,
        "ui_payload": {
            "cards": cards,
            "action_chips": ["View Cart", "Top Deals"],
            "relaxation_level": relaxation_level,
        },
        "current_node": "search_agent_node",
    }


__all__ = ["search_agent_node"]
