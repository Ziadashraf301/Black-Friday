"""
Retrieval Aggregator & Fan-In Barrier Node (Phase 4).
Synchronizes parallel specialist worker nodes, aggregates multi-intent document evidence,
and compiles unified UI payloads with product images and action chips prior to synthesis.
"""
from typing import Dict, Any, List
from ai.workflow.state import AgentState
from core.logging import get_logger

logger = get_logger(__name__)


def retrieval_aggregator_join(state: AgentState) -> Dict[str, Any]:
    """
    Fan-in synchronization barrier in LangGraph multi-intent DAG.
    Gathers intermediate outputs from all active parallel specialist nodes
    and consolidates them into a unified context packet.
    """
    target_intents = state.get("target_intents", [])
    logger.info(f"[GRAPH: AGGREGATOR] Joining parallel branches for intents: {target_intents}")

    consolidated_docs: List[Dict[str, Any]] = []
    ui_cards: List[Dict[str, Any]] = []
    seen_product_ids = set()

    # 1. Gather Search Results
    search_products = state.get("retrieved_products", []) or []
    if search_products:
        consolidated_docs.append({
            "source": "PRODUCT_SEARCH",
            "count": len(search_products),
            "data": search_products,
        })
        for prod in search_products:
            pid = prod.get("product_id")
            if pid and pid not in seen_product_ids:
                seen_product_ids.add(pid)
                ui_cards.append({
                    "type": "PRODUCT_CARD",
                    "product_id": pid,
                    "name": prod.get("name"),
                    "price": prod.get("price"),
                    "image_url": prod.get("image_url", f"/products/{pid}.jpg"),
                    "badge": prod.get("badge", "Catalog Item"),
                })

    # 2. Gather Product Details
    details = state.get("product_details")
    if details:
        consolidated_docs.append({
            "source": "PRODUCT_DETAILS",
            "data": details,
        })
        pid = details.get("product_id")
        if pid:
            existing_card = next((c for c in ui_cards if c.get("product_id") == pid), None)
            if existing_card:
                existing_card["type"] = "PRODUCT_DETAIL_CARD"
                existing_card["materials"] = details.get("materials", [])
                existing_card["care_instructions"] = details.get("care_instructions")
                existing_card["sizes"] = details.get("sizes", [])
                existing_card["stock_status"] = details.get("stock_status", "IN_STOCK")
                if details.get("name"):
                    existing_card["name"] = details.get("name")
                if details.get("price"):
                    existing_card["price"] = details.get("price")
            else:
                seen_product_ids.add(pid)
                ui_cards.append({
                    "type": "PRODUCT_DETAIL_CARD",
                    "product_id": pid,
                    "name": details.get("name"),
                    "price": details.get("price"),
                    "image_url": details.get("image_url", f"/products/{pid}.jpg"),
                    "materials": details.get("materials", []),
                    "care_instructions": details.get("care_instructions"),
                    "sizes": details.get("sizes", []),
                    "stock_status": details.get("stock_status", "IN_STOCK"),
                })

    # 3. Gather Bundle Recommendations
    bundles = state.get("bundle_recommendations", []) or []
    if bundles:
        consolidated_docs.append({
            "source": "BUNDLE_RECOMMENDATIONS",
            "count": len(bundles),
            "data": bundles,
        })
        for b in bundles:
            pid = b.get("product_id")
            if pid:
                existing_card = next((c for c in ui_cards if c.get("product_id") == pid), None)
                if existing_card:
                    existing_card["bundle_price"] = b.get("bundle_price")
                    existing_card["discount_pct"] = b.get("discount_pct", "15% OFF")
                else:
                    seen_product_ids.add(pid)
                    ui_cards.append({
                        "type": "BUNDLE_CARD",
                        "product_id": pid,
                        "name": b.get("name"),
                        "price": b.get("price"),
                        "bundle_price": b.get("bundle_price"),
                        "discount_pct": b.get("discount_pct", "15% OFF"),
                        "image_url": b.get("image_url", f"/products/{pid}.jpg"),
                    })

    # 4. Gather Order & Policy Support
    order_status = state.get("order_status")
    if order_status:
        consolidated_docs.append({
            "source": "ORDER_STATUS",
            "data": order_status,
        })

    policy_details = state.get("policy_details")
    if policy_details:
        consolidated_docs.append({
            "source": "POLICY_DETAILS",
            "data": policy_details,
        })

    # 5. Gather Cart State
    cart = state.get("cart")
    if cart:
        consolidated_docs.append({
            "source": "CART_STATE",
            "data": cart,
        })

    # Compile unified UI payload
    unified_ui_payload = {
        "cards": ui_cards,
        "action_chips": ["View Cart", "Checkout Now", "Top Deals"],
        "relaxation_level": state.get("relaxation_level", "STRICT"),
    }

    logger.info(f"[GRAPH: AGGREGATOR] Consolidated {len(consolidated_docs)} doc sources and {len(ui_cards)} UI cards")

    return {
        "retrieved_docs": consolidated_docs,
        "ui_payload": unified_ui_payload,
        "current_node": "retrieval_aggregator_join",
    }


__all__ = ["retrieval_aggregator_join"]
