import pytest
from unittest.mock import patch, MagicMock
from apps.reflex_app.reflex_app.state import ShoppingState, API_BASE_URL


def test_api_base_url():
    assert API_BASE_URL == "http://127.0.0.1:8000"


@patch("httpx.Client.get")
def test_analytics_fetch_in_reflex(mock_get):
    mock_resp = MagicMock()
    mock_resp.status_code = 200
    mock_resp.json.return_value = {
        "total_orders": 550068,
        "total_revenue": 5095812740.0,
        "avg_order_value": 9264.12,
        "user_count": 5891
    }
    mock_get.return_value = mock_resp

    state = ShoppingState()
    assert state.is_authenticated is False
    assert state.welcome_name == "Shopper"
