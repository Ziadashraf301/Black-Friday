from fastapi.testclient import TestClient

from apps.api.main import app
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
