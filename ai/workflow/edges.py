"""
LangGraph Conditional Edges & Routing Logic (Phase 3).
Maps System-1 RoutingDecision outputs directly to specialized worker agent nodes.
"""
from typing import List, Union
from ai.workflow.state import AgentState
from ai.schemas import IntentType
from core.logging import get_logger

logger = get_logger(__name__)


def route_after_guardrail(state: AgentState) -> List[str]:
    """
    Evaluates state after guardrail_router_node and branches conditionally.
    Supports multi-intent parallel fan-out (returns List[str] of target specialist nodes):
      - Adversarial Attack -> ['refusal_node']
      - Out-of-Domain -> ['steering_node']
      - Shopping Intents -> 1 to N Target Specialist Worker Nodes
    """
    # 0. Cache hit check -> direct to END (sub-1ms)
    from langgraph.graph import END
    if state.get("is_cache_hit"):
        logger.info("[EDGE: ROUTE] Fast-path Tier 0 cache hit routed directly to END")
        return [END]

    # 1. Safety check
    if not state.get("is_safe", True) or state.get("adversarial_prob", 0.0) >= 0.80:
        logger.warning("[EDGE: ROUTE] Routing to refusal_node")
        return ["refusal_node"]

    intent = state.get("intent")

    # 2. Domain check
    if intent == IntentType.OUT_OF_DOMAIN.value:
        logger.info("[EDGE: ROUTE] Routing to steering_node")
        return ["steering_node"]

    # 3. Intent-Specific Multi-Specialist Routing
    target_intents = state.get("target_intents") or ([intent] if intent else [])
    
    node_map = {
        IntentType.PRODUCT_SEARCH.value: "search_agent_node",
        IntentType.DEALS_PROMOTIONS.value: "search_agent_node",
        IntentType.PRODUCT_DETAILS.value: "details_agent_node",
        IntentType.BUNDLE_RECOMMENDATIONS.value: "bundle_agent_node",
        IntentType.CART_ACTIONS.value: "cart_agent_node",
        IntentType.ORDER_SUPPORT.value: "support_agent_node",
    }

    target_nodes: List[str] = []
    for it in target_intents:
        node_name = node_map.get(it)
        if node_name and node_name not in target_nodes:
            target_nodes.append(node_name)

    if not target_nodes:
        target_nodes = ["search_agent_node"]

    logger.info(f"[EDGE: ROUTE] Parallel specialist dispatch: {target_nodes}")
    return target_nodes
