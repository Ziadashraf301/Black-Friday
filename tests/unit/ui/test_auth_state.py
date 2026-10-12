import os
from unittest.mock import MagicMock, patch

from reflex_app.state import API_BASE_URL, CartItem, ShoppingState


def test_auth_token_persisted_with_rx_local_storage():
    """Verify auth_token is configured as rx.LocalStorage client storage."""
    assert ShoppingState._is_client_storage("auth_token") is True
    assert "auth_token" in ShoppingState.vars


def test_token_restored_into_state_on_load_populates_session():
    """Verify that when auth_token is restored from LocalStorage, load_catalog restores user session and member prices."""
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = "stored_jwt_token_abc"
    state.user_id = 0
    state.user_name = ""

    me_response = {
        "user_id": 99,
        "name": "Jane Doe",
        "email": "jane@example.com",
        "gender": "F",
        "age": "26-35",
        "city_category": "A",
        "occupation": 4,
        "cluster_id": 2,
        "cluster_persona": "Vintage Connoisseur",
    }
    catalog_response = [
        {"product_id": "P01", "name": "Item 1", "discounted_price": 50.0, "is_hero": True, "product_category_1": 1}
    ]
    batch_quotes_response = {
        "quotes": [{"product_id": "P01", "predicted_usd": 39.0}],
        "cached_count": 0,
        "predicted_count": 1,
    }

    def mock_get(url, *args, **kwargs):
        mock_resp = MagicMock()
        mock_resp.status_code = 200
        if "/auth/me" in url:
            headers = kwargs.get("headers", {})
            assert headers.get("Authorization") == "Bearer stored_jwt_token_abc"
            mock_resp.json.return_value = me_response
            return mock_resp
        elif "/shopper/curated-catalog" in url:
            mock_resp.json.return_value = catalog_response
            return mock_resp
        return mock_resp

    def mock_post(url, *args, **kwargs):
        mock_resp = MagicMock()
        mock_resp.status_code = 200
        if "/shopper/predict-price-batch" in url:
            headers = kwargs.get("headers", {})
            assert headers.get("Authorization") == "Bearer stored_jwt_token_abc"
            mock_resp.json.return_value = batch_quotes_response
            return mock_resp
        return mock_resp

    with patch("httpx.Client.get", side_effect=mock_get), patch("httpx.Client.post", side_effect=mock_post):
        state.load_catalog()

    assert state.user_id == 99
    assert state.user_name == "Jane Doe"
    assert state.user_email == "jane@example.com"
    assert state.user_gender == "F"
    assert state.user_cluster_persona == "Vintage Connoisseur"
    assert state.is_authenticated is True
    assert state.cached_price_estimates.get("P01") == 39.0


def test_logout_clears_token_and_resets_session():
    """Verify that logging out clears auth_token, resets user state, and clears cached prices."""
    state = ShoppingState(_reflex_internal_init=True)
    state.auth_token = "active_token_123"
    state.user_id = 99
    state.user_name = "Jane Doe"
    state.user_email = "jane@example.com"
    state.cached_price_estimates = {"P01": 39.0}
    state.cart_items = [
        CartItem(
            key="P01_M",
            product_id="P01",
            name="Item 1",
            price=50.0,
            personalized_price=39.0,
            has_personalized=True,
            size="M",
            quantity=1,
            product_category_1=1,
        )
    ]

    assert state.is_authenticated is True

    state.do_logout()

    assert str(state.auth_token) == ""
    assert state.is_authenticated is False
    assert state.user_id == 0
    assert state.user_name == ""
    assert state.user_email == ""
    assert state.cached_price_estimates == {}
    assert len(state.cart_items) == 1
    assert state.cart_items[0].has_personalized is False
    assert state.cart_items[0].personalized_price == 0.0


def test_api_base_url_reads_from_environment():
    """Verify API_BASE_URL is dynamically configurable and has no static hardcoding."""
    from reflex_app.state import API_BASE_URL as CURRENT_URL

    expected = os.getenv("API_BASE_URL", "http://127.0.0.1:8000")
    assert CURRENT_URL == expected


def test_api_base_url():
    """Verify API_BASE_URL matches os.getenv configuration (F-19)."""
    expected = os.getenv("API_BASE_URL", "http://127.0.0.1:8000")
    assert API_BASE_URL == expected


def test_analytics_fetch_in_reflex():
    """Verify state.load_dashboard() executes real method, calls /analytics/summary, and parses results."""
    mock_summary = {"total_orders": 550068, "total_revenue": 5095812740.0, "avg_order_value": 9264.12, "user_count": 5891}
    mock_demographics = [
        {"category": "M", "order_count": 3000, "avg_purchase": 8000.0},
        {"category": "F", "order_count": 2000, "avg_purchase": 7500.0},
    ]

    def mock_get(url, *args, **kwargs):
        resp = MagicMock()
        resp.status_code = 200
        if "/analytics/summary" in url:
            resp.json.return_value = mock_summary
            return resp
        elif "/analytics/demographics" in url:
            resp.json.return_value = mock_demographics
            return resp
        return resp

    state = ShoppingState(_reflex_internal_init=True)
    with patch("httpx.Client.get", side_effect=mock_get):
        state.load_dashboard()

    assert state.is_dashboard_loading is False
    assert state.dashboard_summary == mock_summary
    assert state.dashboard_orders_display == "550,068"
    assert "M" in state.dashboard_revenue_display or "$" in state.dashboard_revenue_display
    assert state.dashboard_users_display == "5,891"
    assert "gender" in state.dashboard_cache
