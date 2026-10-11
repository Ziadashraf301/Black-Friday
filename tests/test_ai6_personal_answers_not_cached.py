"""AI-6: answers that contain a shopper's cart or order data are never written to the shared caches."""

import pytest

import ai.services.cache_service as cache_module
from ai.services.synthesis_service import SynthesisService

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
    _synth(query="what is the return policy", intent="ORDER_SUPPORT", policy_details={"title": "Returns", "summary": "30 days"})
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
