"""
Guardrail & Router Root Entry Node for LangGraph (Phase 3).
Executes Phase 2 GuardrailService (Tier-0 Regex + Jev System-1 API) in single pass.
Enforces security, extracts shopping constraints, and prepares routing decision.
"""
from typing import Dict, Any
from langchain_core.messages import HumanMessage
from ai.workflow.state import AgentState
from ai.services.guardrail_service import guardrail_service
from core.logging import get_logger

logger = get_logger(__name__)


def guardrail_router_node(state: AgentState) -> Dict[str, Any]:
    """
    Root entry node in LangGraph StateGraph.
    Evaluates safety, classifies intent, extracts entities, and sets steering responses.
    """
    query = state.get("query", "")
    if not query and state.get("messages"):
        # Extract last human message text
        for msg in reversed(state["messages"]):
            if isinstance(msg, HumanMessage) or getattr(msg, "type", "") == "human":
                query = msg.content
                break

    user_id = state.get("user_id", "guest_user")
    session_id = state.get("session_id", "default_session")

    logger.info(f"[GRAPH: ROUTER-NODE] Evaluating query: '{query[:60]}...' for user={user_id}")

    # Stateful cart mutations and bypass_cache must never return cached responses
    import re
    is_cart_action = bool(re.search(r"(?i)\b(?:add\s+to\s+cart|put\s+in\s+my\s+cart|remove\s+from\s+cart|clear\s+cart|change\s+size)\b", query))
    should_check_cache = not state.get("bypass_cache") and not is_cart_action

    if should_check_cache:
        # 1. Tier-0: 6-Hour Exact LLM Response Cache Check (<1ms)
        from ai.services.cache_service import cache_service
        cached_response = cache_service.get_exact_llm_response(raw_query=query)
        if cached_response:
            logger.info(f"[GRAPH: ROUTER-NODE] Tier-0 6h cache HIT for query='{query[:40]}'")
            cached_intents = cached_response.get("target_intents") or ["PRODUCT_SEARCH"]
            cached_intent = cached_intents[0] if cached_intents else "PRODUCT_SEARCH"
            from langchain_core.messages import AIMessage
            return {
                "query": query,
                "intent": cached_intent,
                "target_intents": cached_intents,
                "is_safe": True,
                "adversarial_prob": 0.0,
                "steering_response": None,
                "routing_latency_ms": 0.4,
                "entities": {},
                "decomposed_entities": {},
                "is_cache_hit": True,
                "cache_tier": "TIER_0_HARD_6H",
                "final_response": cached_response.get("response_message"),
                "ui_payload": cached_response.get("ui_payload", {}),
                "retrieved_products": cached_response.get("retrieved_products", []),
                "product_details": cached_response.get("product_details"),
                "bundle_recommendations": cached_response.get("bundle_recommendations", []),
                "order_status": cached_response.get("order_status"),
                "policy_details": cached_response.get("policy_details"),
                "messages": [AIMessage(content=cached_response.get("response_message", ""))],
                "current_node": "guardrail_router_node",
            }

        # 2. Tier-1: True Vector Semantic Cache Check (pgvector HNSW Cosine Sim >= 0.92, <5ms)
        semantic_cached = cache_service.get_vector_semantic_response(raw_query=query)
        if semantic_cached:
            sim = semantic_cached.get("cosine_similarity", 0.95)
            logger.info(f"[GRAPH: ROUTER-NODE] Tier-1 Vector Semantic cache HIT for query='{query[:40]}' (sim={sim})")
            from langchain_core.messages import AIMessage
            intent = semantic_cached.get("intent", "PRODUCT_SEARCH")
            return {
                "query": query,
                "intent": intent,
                "target_intents": [intent],
                "is_safe": True,
                "adversarial_prob": 0.0,
                "steering_response": None,
                "routing_latency_ms": 2.5,
                "entities": {},
                "decomposed_entities": {},
                "is_cache_hit": True,
                "cache_tier": "TIER_1_SEMANTIC_VECTOR",
                "final_response": semantic_cached.get("response_message"),
                "ui_payload": semantic_cached.get("ui_payload", {}),
                "retrieved_products": semantic_cached.get("retrieved_products", []),
                "product_details": semantic_cached.get("product_details"),
                "bundle_recommendations": semantic_cached.get("bundle_recommendations", []),
                "order_status": semantic_cached.get("order_status"),
                "policy_details": semantic_cached.get("policy_details"),
                "messages": [AIMessage(content=semantic_cached.get("response_message", ""))],
                "current_node": "guardrail_router_node",
            }


    # 3. Orchestrate Phase 2 Guardrail Service
    response = guardrail_service.evaluate_query(
        query=query,
        user_id=user_id,
        session_id=session_id,
    )

    # 3. Emit MLflow GenAI Trace
    try:
        from ai.observability.tracing import agent_tracer
        agent_tracer.trace_guardrail_eval(
            query=query,
            user_id=user_id,
            session_id=session_id,
            is_safe=response.is_safe,
            adversarial_prob=response.adversarial_probability,
            target_intents=[t.value if hasattr(t, "value") else str(t) for t in response.target_intents],
        )
    except Exception as trace_err:
        logger.debug(f"[ROUTER: TRACING] Trace emission skipped: {trace_err}")

    return {
        "query": query,
        "intent": response.intent,
        "target_intents": response.target_intents,
        "is_safe": response.is_safe,
        "adversarial_prob": response.adversarial_probability,
        "steering_response": response.steering_response,
        "routing_latency_ms": response.latency_ms,
        "entities": response.entities.model_dump(),
        "decomposed_entities": response.decomposed_entities,
        "current_node": "guardrail_router_node",
    }
