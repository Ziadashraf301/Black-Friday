import pytest
from unittest.mock import MagicMock
from fastapi.testclient import TestClient
from apps.api.main import app
from apps.api.routes import shopper
from core.security import create_access_token


def test_shopper_predict_price_batch_unauthenticated(monkeypatch):
    mock_quotes = [
        {
            "product_id": "P000001",
            "predicted_usd": 42.50,
            "catalog_price": 50.0,
            "normalized_prediction": 0.5,
            "model_used": "mock-model",
        }
    ]
    monkeypatch.setattr(shopper.shopper_service, "estimate_price_batch", lambda **kwargs: mock_quotes)

    client = TestClient(app)
    payload = {
        "items": [
            {
                "product_id": "P000001",
                "product_category_1": 1,
                "product_category_2": 2,
                "product_category_3": 3,
            }
        ],
    }

    # Should succeed without Authorization header (guest caller)
    resp = client.post("/shopper/predict-price-batch", json=payload)
    assert resp.status_code == 200
    data = resp.json()
    assert "quotes" in data
    assert len(data["quotes"]) == 1
    assert data["quotes"][0]["product_id"] == "P000001"


def test_shopper_predict_price_rate_limit(monkeypatch):
    mock_quote = {
        "product_id": "P000001",
        "predicted_usd": 42.50,
        "catalog_price": 50.0,
        "normalized_prediction": 0.5,
        "model_used": "mock-model",
    }
    monkeypatch.setattr(shopper.shopper_service, "estimate_price", lambda **kwargs: mock_quote)

    token = create_access_token({"sub": "777", "email": "shopper@test.com"})

    import apps.api.rate_limiting.rate_limiter as rl_module
    from apps.api.rate_limiting.rate_limiter import RedisRateLimiter
    strict_limiter = RedisRateLimiter(minute_limit=1, day_limit=10)
    strict_limiter._custom_client = None
    strict_limiter._in_memory_windows = {}
    monkeypatch.setattr(rl_module, "rate_limiter", strict_limiter)

    client = TestClient(app)
    payload = {
        "product_id": "P000001",
        "product_category_1": 1,
        "product_category_2": 2,
        "product_category_3": 3,
    }

    headers = {"Authorization": f"Bearer {token}"}
    resp1 = client.post("/shopper/predict-price", json=payload, headers=headers)
    assert resp1.status_code == 200

    # 2nd request hits rate limit
    resp2 = client.post("/shopper/predict-price", json=payload, headers=headers)
    assert resp2.status_code == 429
