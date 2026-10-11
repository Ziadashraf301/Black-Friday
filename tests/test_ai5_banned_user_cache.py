"""AI-5: a locked-out user is refused even when their query has a cached answer."""

from unittest.mock import MagicMock

import pytest

from ai.guardrails.strike_tracker import StrikeTracker
from ai.nodes import router_node as router_module
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
