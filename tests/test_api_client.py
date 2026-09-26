import pytest
from unittest.mock import patch, MagicMock
from apps.ui.api_client import APIClient


@pytest.fixture
def api_client():
    return APIClient(base_url="http://localhost:8000")


@patch("requests.get")
def test_get_health(mock_get, api_client):
    mock_resp = MagicMock()
    mock_resp.status_code = 200
    mock_resp.json.return_value = {"status": "healthy", "service": "Black-Friday-v2"}
    mock_get.return_value = mock_resp

    res = api_client.get_health()
    assert res["status"] == "healthy"
    mock_get.assert_called_once_with(
        "http://localhost:8000/health",
        params=None,
        headers={"Content-Type": "application/json"},
        timeout=10
    )


@patch("requests.get")
def test_get_analytics_summary(mock_get, api_client):
    mock_resp = MagicMock()
    mock_resp.status_code = 200
    mock_resp.json.return_value = {"total_orders": 550068, "total_revenue": 5095812740.0}
    mock_get.return_value = mock_resp

    res = api_client.get_analytics_summary()
    assert res["total_orders"] == 550068


@patch("requests.post")
def test_predict_shopper_price(mock_post, api_client):
    mock_resp = MagicMock()
    mock_resp.status_code = 200
    mock_resp.json.return_value = {"predicted_usd": 8500.0, "normalized_prediction": 0.397}
    mock_post.return_value = mock_resp

    payload = {"product_category_1": 3, "product_category_2": 4, "product_category_3": 12, "product_id": "P00069042"}
    res = api_client.predict_shopper_price(payload)
    assert res["predicted_usd"] == 8500.0
    mock_post.assert_called_once_with(
        "http://localhost:8000/shopper/predict-price",
        json=payload,
        headers={"Content-Type": "application/json"},
        timeout=15
    )


@patch("requests.post")
def test_auth_login_and_bearer_token(mock_post, api_client):
    mock_resp = MagicMock()
    mock_resp.status_code = 200
    mock_resp.json.return_value = {
        "access_token": "mock-token-xyz",
        "user_id": 42,
        "name": "Jane Doe",
        "cluster_persona": "Single females <= 50"
    }
    mock_post.return_value = mock_resp

    res = api_client.login("jane@example.com", "secretpass")
    assert res["access_token"] == "mock-token-xyz"
    api_client.set_token("mock-token-xyz")
    assert api_client._headers()["Authorization"] == "Bearer mock-token-xyz"


@patch("requests.get")
def test_get_eda_with_stats(mock_get, api_client):
    mock_resp = MagicMock()
    mock_resp.status_code = 200
    mock_resp.json.return_value = {
        "dimension": "gender",
        "categories": [{"category": "M", "order_count": 400000}],
        "test_name": "Welch's Two-Sample t-test",
        "test_statistic": -46.358,
        "p_value": 1e-16,
        "is_significant": True,
        "interpretation": "Statistically Significant",
        "details": {}
    }
    mock_get.return_value = mock_resp

    res = api_client.get_eda_with_stats("gender")
    assert res["is_significant"] is True
    assert res["dimension"] == "gender"
