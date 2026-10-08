import os
import importlib
import pytest
from unittest.mock import patch, MagicMock

from core.config import settings
import sys
sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))
import reflex_app.state as state_mod
from reflex_app.state import ShoppingState, API_BASE_URL


def test_api_base_url():
    """Verify API_BASE_URL matches os.getenv configuration (F-19)."""
    expected = os.getenv("API_BASE_URL", "http://127.0.0.1:8000")
    assert API_BASE_URL == expected


def test_analytics_fetch_in_reflex():
    """Verify state.load_dashboard() executes real method, calls /analytics/summary, and parses results."""
    mock_summary = {
        "total_orders": 550068,
        "total_revenue": 5095812740.0,
        "avg_order_value": 9264.12,
        "user_count": 5891
    }
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
