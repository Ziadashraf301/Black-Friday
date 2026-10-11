import pytest
from fastapi.testclient import TestClient

import apps.api.rate_limiting.rate_limiter as rl_module
from apps.api.main import app
from apps.api.rate_limiting.rate_limiter import RedisRateLimiter
from apps.api.routes import shopper
from core.security import create_access_token

client = TestClient(app)


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
