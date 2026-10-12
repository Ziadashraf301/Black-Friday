from fastapi.testclient import TestClient

from apps.api.main import app
from apps.api.routes import shopper
from core.security import create_access_token

client = TestClient(app)

ACCOUNT = {
    "sub": "4242",
    "gender": "F",
    "age": "36-45",
    "city_category": "C",
    "marital_status": 1,
    "occupation": 7,
    "stay_in_current_city_years": "3",
}
SPOOF = {
    "gender": "M",
    "age": "0-17",
    "city_category": "A",
    "marital_status": 0,
    "occupation": 1,
    "stay_in_current_city_years": "0",
}


def _headers():
    return {"Authorization": f"Bearer {create_access_token(ACCOUNT)}"}


def test_predict_batch_ignores_body_demographics(monkeypatch):
    seen = {}

    def fake(**kw):
        seen.update(kw)
        return []

    monkeypatch.setattr(shopper.shopper_service, "estimate_price_batch", fake)
    body = {"items": [{"product_id": "P1", **SPOOF}]}
    assert client.post("/shopper/predict-price-batch", json=body, headers=_headers()).status_code == 200
    assert all(k not in seen["items"][0] for k in SPOOF)
    assert seen["user_demographics"]["gender"] == "F"
    assert seen["user_demographics"]["age"] == "36-45"


def test_purchase_uses_account_demographics(monkeypatch):
    seen = {}

    def fake(**kw):
        seen.update(kw)
        return {"id": 1, "product_id": "P1", "predicted_usd": 1.0, "model_used": "m", "purchased_at": "now"}

    monkeypatch.setattr(shopper.shopper_service, "process_purchase", fake)
    resp = client.post("/shopper/purchase", json={"product_id": "P1", **SPOOF}, headers=_headers())
    assert resp.status_code == 200
    for key, val in ACCOUNT.items():
        if key != "sub":
            assert seen[key] == val


def test_batch_purchase_ignores_item_demographics(monkeypatch):
    seen = {}

    def fake(**kw):
        seen.update(kw)
        return []

    monkeypatch.setattr(shopper.shopper_service, "process_batch_purchase", fake)
    body = {"items": [{"product_id": "P1", "quantity": 2, **SPOOF}]}
    assert client.post("/shopper/purchase/batch", json=body, headers=_headers()).status_code == 200
    assert seen["items"][0]["quantity"] == 2
    assert all(k not in seen["items"][0] for k in SPOOF)
    assert seen["user_demographics"]["gender"] == "F"
