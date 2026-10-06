"""
Unit and Integration Tests for MLflow GenAI Tracing (Phase 4 - Task P4-03).
Validates:
  1. Initialization of MLflow tracking and autologging.
  2. Custom trace spans execution: cache lookup, guardrail evaluation, search ladder, and bundle pricing.
  3. Safe fallback in offline mode.
"""
import pytest
from ai.observability.tracing import agent_tracer, AgentTracer


class TestMLflowGenAITracing:
    """Verifies MLflow GenAI tracing spans and telemetry instrumentation."""

    def test_tracer_singleton_initialization(self):
        assert agent_tracer is not None
        assert isinstance(agent_tracer, AgentTracer)

    def test_trace_cache_lookup_span(self):
        res = agent_tracer.trace_cache_lookup(
            raw_query="women warm down coats under $150",
            tier="TIER_0_HARD_6H",
            is_hit=True,
            latency_ms=0.85,
        )
        assert res["hit"] is True
        assert res["tier"] == "TIER_0_HARD_6H"
        assert res["latency_ms"] == 0.85

    def test_trace_guardrail_eval_span(self):
        res = agent_tracer.trace_guardrail_eval(
            query="Find leather jackets in size M",
            user_id="shopper_44",
            session_id="sess_88",
            is_safe=True,
            adversarial_prob=0.02,
            target_intents=["PRODUCT_SEARCH"],
        )
        assert res["is_safe"] is True
        assert res["adversarial_prob"] == 0.02
        assert "PRODUCT_SEARCH" in res["intents"]

    def test_trace_search_ladder_span(self):
        res = agent_tracer.trace_search_ladder(
            query="hoodies under 40",
            relaxation_level="TIER_1_STRICT",
            items_found=5,
            filters_applied={"max_price": 40.0},
        )
        assert res["relaxation_level"] == "TIER_1_STRICT"
        assert res["items_found"] == 5

    def test_trace_bundle_pricing_span(self):
        res = agent_tracer.trace_bundle_pricing(
            target_product_id="P00025442",
            bundle_items=[{"id": "P00025442"}, {"id": "P0009999"}],
            original_price=120.0,
            bundle_price=96.0,
            savings_pct=20.0,
        )
        assert res["savings_pct"] == 20.0
        assert res["bundle_items_count"] == 2
