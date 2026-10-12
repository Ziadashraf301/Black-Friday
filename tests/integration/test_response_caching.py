"""AI-5: a locked-out user is refused even when their query has a cached answer.

AI-6: answers that contain a shopper's cart or order data are never written to the shared caches.

AI-7: `bypass_cache` is part of AgentState, so LangGraph delivers it to the router node instead of dropping it.

AI-8: cache and LLM failures that silently degrade the answer are logged at warning, not debug."""

from unittest.mock import MagicMock

import pytest
from langgraph.graph import END, StateGraph

import ai.services.cache_service as cache_module
from ai.guardrails.strike_tracker import StrikeTracker
from ai.nodes import router_node as router_module
from ai.schemas.state import AgentState
from ai.services.cache_service import TwoTierCacheService
from ai.services.synthesis_service import SynthesisService
from ai.workflow.edges import route_after_guardrail

BANNED = "ai5_banned_user"
QUERY = "show me leather jackets under $150"


@pytest.fixture
def tracker(monkeypatch):
    tracker = StrikeTracker(cache=MagicMock(is_available=False))
    monkeypatch.setattr(router_module, "strike_tracker", tracker)
    # the guardrail service imports the same singleton lazily
    import ai.guardrails.strike_tracker as strike_module

    monkeypatch.setattr(strike_module, "strike_tracker", tracker)
    return tracker


@pytest.fixture
def cached_answer(monkeypatch):
    import ai.services.cache_service as cache_module

    hit = {"response_message": "cached answer", "target_intents": ["PRODUCT_SEARCH"], "ui_payload": {}}
    monkeypatch.setattr(cache_module.cache_service, "get_exact_llm_response", lambda *a, **k: dict(hit))
    monkeypatch.setattr(cache_module.cache_service, "get_vector_semantic_response", lambda *a, **k: None)


def _run(user_id):
    return router_module.guardrail_router_node({"query": QUERY, "user_id": user_id, "session_id": "s"})


def test_unbanned_user_gets_cache_hit(tracker, cached_answer):
    out = _run("ai5_fine_user")
    assert out["is_cache_hit"] is True
    assert out["final_response"] == "cached answer"


def test_banned_user_skips_cache_and_is_refused(tracker, cached_answer):
    for _ in range(tracker.MAX_STRIKES):
        tracker.record_strike(BANNED)
    assert tracker.is_banned(BANNED)

    out = _run(BANNED)
    assert not out.get("is_cache_hit")
    assert out["is_safe"] is False
    assert route_after_guardrail(out) == ["refusal_node"]


CART = {"item_count": 1, "items": [{"name": "Jacket", "size": "M", "quantity": 1, "total_price": 50.0}], "subtotal": 50.0}
ORDER = {"order_id": "ORD-1", "status": "Shipped", "carrier": "FedEx", "tracking_number": "FX1"}


@pytest.fixture
def stores(monkeypatch):
    calls = []
    monkeypatch.setattr(cache_module.cache_service, "store_exact_llm_response", lambda **kw: calls.append(("exact", kw)))
    monkeypatch.setattr(cache_module.cache_service, "store_vector_semantic_response", lambda **kw: calls.append(("vec", kw)))
    return calls


def _synth(**state):
    base = {"query": "show me jackets", "intent": "PRODUCT_SEARCH", "user_id": "42", "session_id": "s"}
    return SynthesisService(llm_client=None).synthesize({**base, **state})


def test_catalog_answer_is_cached(stores):
    _synth()
    assert {kind for kind, _ in stores} == {"exact", "vec"}


def test_policy_answer_is_cached(stores):
    _synth(
        query="what is the return policy", intent="ORDER_SUPPORT", policy_details={"title": "Returns", "summary": "30 days"}
    )
    assert {kind for kind, _ in stores} == {"exact", "vec"}


@pytest.mark.parametrize(
    "state",
    [
        {"intent": "CART_ACTIONS", "cart": CART},
        {"intent": "ORDER_SUPPORT", "order_status": ORDER},
        # multi-intent: the first intent is a search but the answer also carries the cart / order
        {"intent": "PRODUCT_SEARCH", "target_intents": ["PRODUCT_SEARCH", "CART_ACTIONS"], "cart": CART},
        {"intent": "PRODUCT_SEARCH", "target_intents": ["PRODUCT_SEARCH", "ORDER_SUPPORT"], "order_status": ORDER},
        {"intent": "PRODUCT_SEARCH", "order_status": ORDER},
    ],
)
def test_personal_answer_is_not_cached(stores, state):
    _synth(**state)
    assert stores == []


def test_bypass_cache_reaches_nodes():
    seen = {}

    def node(state):
        seen["bypass_cache"] = state.get("bypass_cache")
        return {}

    graph = StateGraph(AgentState)
    graph.add_node("n", node)
    graph.set_entry_point("n")
    graph.add_edge("n", END)
    graph.compile().invoke({"query": "q", "bypass_cache": True})
    assert seen["bypass_cache"] is True


def _service(repo):
    return TwoTierCacheService(cache_manager_instance=MagicMock(is_available=False), repo=repo)


def test_vector_lookup_failure_is_a_warning(caplog):
    repo = MagicMock()
    repo.find_semantic_cached_response.side_effect = RuntimeError("db down")
    with caplog.at_level("WARNING"):
        assert _service(repo).get_vector_semantic_response("q", "u", query_vec=[0.0]) is None
    assert any(r.levelname == "WARNING" and "db down" in r.getMessage() for r in caplog.records)


def test_vector_store_failure_is_a_warning(caplog):
    repo = MagicMock()
    repo.save_semantic_cache_entry.side_effect = RuntimeError("insert failed")
    with caplog.at_level("WARNING"):
        assert _service(repo).store_vector_semantic_response("q", "u", "r", {}, "I", query_vec=[0.0]) is False
    assert any(r.levelname == "WARNING" and "insert failed" in r.getMessage() for r in caplog.records)


def test_llm_failure_falls_back_to_template_with_a_warning(caplog):
    client = MagicMock()
    client.models.generate_content.side_effect = RuntimeError("quota exceeded")
    with caplog.at_level("WARNING"):
        assert SynthesisService(llm_client=client).generate_llm_rag("ctx", "q") is None
    assert any(r.levelname == "WARNING" and "quota exceeded" in r.getMessage() for r in caplog.records)
