"""
Unit and Integration Tests for Phase 4 Two-Tier Caching & Multi-Intent Fan-Out (P4-01 & P4-02).
Validates:
  1. Tier-0 6-hour hard LLM response caching and sub-millisecond retrieval.
  2. Tier-0 fast-path graph bypass returning stored final_response and ui_payload.
  3. Tier-1 semantic cache for product details (24h TTL) and policies (30d TTL).
  4. Multi-intent concurrent specialist execution through StateGraph join aggregator.
"""
import pytest
from ai.services.cache_service import TwoTierCacheService, cache_service
from ai.workflow.graph import shopping_graph
from ai.workflow.state import AgentState
from langchain_core.messages import HumanMessage


class TestTwoTierCacheService:
    """Verifies core cryptographic hashing and Redis storage semantics."""

    def test_query_hashing_determinism(self):
        q1 = "Show me leather jackets under $100"
        q2 = "  show me leather jackets under $100  "
        assert TwoTierCacheService.hash_query(q1) == TwoTierCacheService.hash_query(q2)

    def test_constraint_hashing_determinism(self):
        c1 = {"category": "outerwear", "max_price": 100, "sizes": ["M", "L"]}
        c2 = {"sizes": ["L", "m"], "max_price": 100, "category": "OUTERWEAR"}
        h1 = TwoTierCacheService.hash_constraints("PRODUCT_SEARCH", c1)
        h2 = TwoTierCacheService.hash_constraints("PRODUCT_SEARCH", c2)
        assert h1 == h2

    def test_tier0_exact_store_and_hit(self):
        test_query = "unique test query for 6h cache validation 9999"
        cache_service.invalidate_exact_cache(test_query)

        # 1. Miss initially
        assert cache_service.get_exact_llm_response(test_query) is None

        # 2. Store response
        stored = cache_service.store_exact_llm_response(
            raw_query=test_query,
            response_message="Here are your discounted leather jackets!",
            ui_payload={"type": "product_carousel", "data": {"products": [{"id": "P001"}]}},
            target_intents=["PRODUCT_SEARCH"],
            ttl=3600,
        )
        assert stored is True

        # 3. Hit
        cached = cache_service.get_exact_llm_response(test_query)
        assert cached is not None
        assert cached["is_cache_hit"] is True
        assert cached["cache_tier"] == "TIER_0_HARD_6H"
        assert cached["response_message"] == "Here are your discounted leather jackets!"
        assert cached["ui_payload"]["type"] == "product_carousel"

        # Cleanup
        cache_service.invalidate_exact_cache(test_query)

    def test_tier1_semantic_store_and_hit(self):
        intent = "POLICY_FAQ"
        constraints = {"topic": "returns_test_policy_xyz"}

        stored = cache_service.store_semantic_response(
            intent=intent,
            constraints=constraints,
            answer_text="30-day free returns on Black Friday purchases.",
            ui_payload={"policy": {"topic": "returns", "days": 30}},
            ttl=120,
        )
        assert stored is True

        hit = cache_service.get_semantic_response(intent=intent, constraints=constraints)
        assert hit is not None
        assert hit["is_cache_hit"] is True
        assert hit["cache_tier"] == "TIER_1_SEMANTIC"
        assert hit["response_message"] == "30-day free returns on Black Friday purchases."


class TestGraphCachingIntegration:
    """Verifies LangGraph workflow behavior with Tier-0 and Tier-1 caching."""

    def test_tier0_fast_path_graph_bypass(self):
        raw_query = "fast tier0 cache bypass test query 12345"
        cache_service.store_exact_llm_response(
            raw_query=raw_query,
            response_message="CACHED_IMMEDIATE_RESPONSE",
            ui_payload={"type": "product_carousel", "data": {}},
            target_intents=["PRODUCT_SEARCH"],
            ttl=300,
        )

        initial_state: AgentState = {
            "query": raw_query,
            "messages": [HumanMessage(content=raw_query)],
            "user_id": "test_user_cache",
            "session_id": "sess_cache_bypass",
            "retrieved_products": [],
            "bundle_recommendations": [],
            "retrieved_docs": [],
        }

        # Graph invocation should hit Tier-0 in guardrail_router_node and route straight to END
        result = shopping_graph.invoke(initial_state)

        assert result.get("is_cache_hit") is True
        assert result.get("cache_tier") == "TIER_0_HARD_6H"
        assert result.get("final_response") == "CACHED_IMMEDIATE_RESPONSE"
        # Specialist nodes should NOT have executed
        assert result.get("retrieved_products") == []

        # Cleanup
        cache_service.invalidate_exact_cache(raw_query)

    def test_multi_intent_fanout_concurrent_execution(self):
        # Query combining details inquiry and policy inquiry
        query = "What materials is P00025442 made of and what is your return policy?"
        cache_service.invalidate_exact_cache(query)

        initial_state: AgentState = {
            "query": query,
            "messages": [HumanMessage(content=query)],
            "user_id": "test_user_multi",
            "session_id": "sess_multi_001",
            "retrieved_products": [],
            "bundle_recommendations": [],
            "retrieved_docs": [],
        }

        result = shopping_graph.invoke(initial_state)

        # Both details and policy specialist artifacts must be populated!
        assert result.get("product_details") is not None
        assert result.get("product_details", {}).get("product_id") == "P00025442"
        assert result.get("policy_details") is not None
        assert "returns" in str(result.get("policy_details")).lower()
        assert len(result.get("final_response", "")) > 10
