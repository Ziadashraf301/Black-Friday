import os
import pytest
from unittest.mock import MagicMock, patch
import httpx

from core.config import settings
import sys
sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))
from reflex_app.state import ShoppingState, CartItem


def test_authenticated_add_to_cart_uses_member_price():
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = "test_token_123"
    state.products = [
        {"product_id": "P100", "name": "Vintage Denim", "discounted_price": 60.0, "product_category_1": 1}
    ]
    state.cached_price_estimates = {"P100": 45.0}

    state.add_to_cart("P100")

    assert len(state.cart_items) == 1
    item = state.cart_items[0]
    assert item.has_personalized is True
    assert item.personalized_price == 45.0
    assert state.cart_personalized_total == 45.0


def test_guest_add_to_cart_uses_catalog_price():
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = ""
    state.products = [
        {"product_id": "P100", "name": "Vintage Denim", "discounted_price": 60.0, "product_category_1": 1}
    ]
    state.cached_price_estimates = {"P100": 45.0}

    state.add_to_cart("P100")

    assert len(state.cart_items) == 1
    item = state.cart_items[0]
    assert item.has_personalized is False
    assert state.cart_personalized_total == 60.0


def test_hero_add_to_cart_authenticated_uses_member_price():
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = "test_token_123"
    state.hero_product = {
        "product_id": "HERO_1",
        "name": "Hero Jacket",
        "discounted_price": 100.0,
        "product_category_1": 2,
    }
    state.cached_price_estimates = {"HERO_1": 80.0}

    state.add_hero_to_cart()

    assert len(state.cart_items) == 1
    item = state.cart_items[0]
    assert item.has_personalized is True
    assert item.personalized_price == 80.0
    assert state.cart_personalized_total == 80.0


def test_checkout_respects_quantity_and_uses_batch_endpoint():
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = "valid_token"
    state.cart_items = [
        CartItem(
            key="P100_M",
            product_id="P100",
            name="Vintage Denim",
            price=60.0,
            personalized_price=45.0,
            has_personalized=True,
            size="M",
            quantity=3,
            product_category_1=1,
        )
    ]

    recorded_requests = []

    def mock_post(url, *args, **kwargs):
        recorded_requests.append((url, kwargs))
        mock_resp = MagicMock()
        mock_resp.status_code = 200
        mock_resp.json.return_value = {
            "purchases": [{"id": 1, "product_id": "P100", "predicted_usd": 45.0}],
            "total_items": 1,
            "total_amount": 45.0,
        }
        return mock_resp

    with patch("httpx.Client.post", side_effect=mock_post), \
         patch.object(ShoppingState, "load_purchase_history"):
        state.checkout()

    assert len(state.cart_items) == 0, "Successful checkout must clear cart"
    assert state.is_cart_open is False
    assert len(recorded_requests) == 1
    url, kwargs = recorded_requests[0]
    assert "/shopper/purchase/batch" in url
    items_sent = kwargs["json"]["items"]
    assert len(items_sent) == 1
    assert items_sent[0]["product_id"] == "P100"
    assert items_sent[0]["quantity"] == 3


def test_checkout_failure_keeps_cart_and_reports_error():
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = "valid_token"
    state.cart_items = [
        CartItem(
            key="P100_M",
            product_id="P100",
            name="Vintage Denim",
            price=60.0,
            quantity=2,
            product_category_1=1,
        )
    ]

    def mock_post_fail(url, *args, **kwargs):
        mock_resp = MagicMock()
        mock_resp.status_code = 500
        mock_resp.json.return_value = {"detail": "Payment gateway timeout"}
        return mock_resp

    with patch("httpx.Client.post", side_effect=mock_post_fail):
        state.checkout()

    assert len(state.cart_items) == 1, "Failed checkout must NOT clear cart"
    assert state.cart_items[0].quantity == 2
    assert "timeout" in state.checkout_error.lower() or "failed" in state.checkout_error.lower()


def test_checkout_fallback_when_batch_endpoint_unavailable():
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = "valid_token"
    state.cart_items = [
        CartItem(
            key="P100_M",
            product_id="P100",
            name="Vintage Denim",
            price=60.0,
            quantity=3,
            product_category_1=1,
        )
    ]

    per_item_calls = []

    def mock_post(url, *args, **kwargs):
        mock_resp = MagicMock()
        if "/purchase/batch" in url:
            mock_resp.status_code = 404
            return mock_resp
        if "/purchase" in url:
            per_item_calls.append(kwargs)
            mock_resp.status_code = 200
            mock_resp.json.return_value = {"id": 1, "product_id": "P100", "predicted_usd": 60.0}
            return mock_resp
        return mock_resp

    with patch("httpx.Client.post", side_effect=mock_post), \
         patch.object(ShoppingState, "load_purchase_history"):
        state.checkout()

    assert len(per_item_calls) == 3, "Fallback must send request per quantity item"
    assert len(state.cart_items) == 0, "Successful fallback checkout must clear cart"
