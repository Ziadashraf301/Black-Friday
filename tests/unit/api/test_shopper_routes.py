from unittest.mock import MagicMock

import pytest
from fastapi.testclient import TestClient

import apps.api.rate_limiting.rate_limiter as rl_module
from apps.api.main import app
from apps.api.rate_limiting.rate_limiter import RedisRateLimiter
from apps.api.routes import shopper
from apps.api.schemas import MAX_BATCH_ITEMS, MAX_ITEM_QUANTITY
from core.security import create_access_token

client = TestClient(app)


def _headers():
    return {"Authorization": f"Bearer {create_access_token({'sub': '5'})}"}


def test_purchase_batch_quantity_capped():
    body = {"items": [{"product_id": "P1", "quantity": MAX_ITEM_QUANTITY + 1}]}
    assert client.post("/shopper/purchase/batch", json=body, headers=_headers()).status_code == 422


def test_purchase_batch_item_count_capped():
    body = {"items": [{"product_id": "P1"}] * (MAX_BATCH_ITEMS + 1)}
    assert client.post("/shopper/purchase/batch", json=body, headers=_headers()).status_code == 422


def test_predict_batch_item_count_capped():
    body = {"items": [{"product_id": "P1"}] * (MAX_BATCH_ITEMS + 1)}
    assert client.post("/shopper/predict-price-batch", json=body, headers=_headers()).status_code == 422




@pytest.mark.parametrize("limit", [0, -1, 501, "abc"])
def test_catalog_limit_bounds_rejected(limit):
    assert client.get(f"/shopper/catalog?limit={limit}").status_code == 422


def test_catalog_limit_in_range_reaches_service(monkeypatch):
    seen = []
    monkeypatch.setattr(shopper.shopper_service, "get_catalog", lambda limit, repo: seen.append(limit) or [])
    assert client.get("/shopper/catalog?limit=500").status_code == 200
    assert seen == [500]


def test_predict_batch_rate_limited(monkeypatch):
    monkeypatch.setattr(shopper.shopper_service, "estimate_price_batch", lambda **kw: [])
    strict = RedisRateLimiter(minute_limit=1, day_limit=10)
    strict._custom_client = None
    strict._in_memory_windows = {}
    monkeypatch.setattr(rl_module, "rate_limiter", strict)

    headers = {"Authorization": f"Bearer {create_access_token({'sub': '9001'})}"}
    body = {"items": [{"product_id": "P1"}]}
    assert client.post("/shopper/predict-price-batch", json=body, headers=headers).status_code == 200
    assert client.post("/shopper/predict-price-batch", json=body, headers=headers).status_code == 429


def test_shopper_predict_price_batch_requires_auth(monkeypatch):
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

    # Guests are rejected (API-3); the Reflex UI only calls this endpoint with a token
    assert client.post("/shopper/predict-price-batch", json=payload).status_code in (401, 403)

    token = create_access_token({"sub": "778", "email": "shopper@test.com"})
    resp = client.post("/shopper/predict-price-batch", json=payload, headers={"Authorization": f"Bearer {token}"})
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
