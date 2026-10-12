"""
Regression test for Batch Purchase Endpoint (POST /shopper/purchase/batch):
- Records all cart items in ONE transaction with quantities
- Returns per-item results
- Single-item endpoint continues working
- Partial failure rolls back everything (atomic rollback)
"""
import uuid
import pytest
from unittest.mock import patch
from fastapi.testclient import TestClient
from sqlalchemy import text

from apps.api.main import app
from apps.api.services.auth_service import auth_service
from apps.api.services.model_service import model_service
from core.db.repository import BlackFridayRepository
from core.db.session import get_db_engine


@pytest.fixture(autouse=True)
def setup_models_and_tables():
    model_service.load_models()
    r = BlackFridayRepository()
    r.create_app_tables()
    return r


@pytest.fixture
def repo(setup_models_and_tables):
    return setup_models_and_tables


@pytest.fixture
def client():
    with TestClient(app) as c:
        yield c


@pytest.fixture
def auth_header(repo):
    """Creates a registered test user and returns their Authorization header and user_id."""
    email = f"shopper_{uuid.uuid4().hex[:8]}@example.com"
    user_data = {
        "name": "Batch Shopper",
        "email": email,
        "password": "securepassword123",
        "gender": "M",
        "age": "26-35",
        "city_category": "B",
        "marital_status": 0,
        "occupation": 4,
        "stay_in_current_city_years": "2",
    }
    reg = auth_service.register_user(user_data, repo=repo)
    token = reg["access_token"]
    user_id = reg["user_id"]
    return {"Authorization": f"Bearer {token}"}, user_id


def test_batch_purchase_success(client, auth_header, repo):
    """Verify batch purchase records all items with quantities in one transaction."""
    headers, user_id = auth_header

    initial_purchases = repo.get_user_purchase_history(user_id)
    initial_count = len(initial_purchases)

    payload = {
        "items": [
            {
                "product_id": "P00025442",
                "product_category_1": 1,
                "product_category_2": 6,
                "product_category_3": 14,
                "quantity": 2,
            },
            {
                "product_id": "P00110742",
                "product_category_1": 1,
                "product_category_2": 2,
                "product_category_3": 8,
                "quantity": 1,
            },
        ]
    }

    response = client.post("/shopper/purchase/batch", json=payload, headers=headers)
    assert response.status_code == 200, response.text
    data = response.json()

    assert "purchases" in data
    assert data["total_items"] == 3  # 2 of P00025442 + 1 of P00110742
    assert len(data["purchases"]) == 3
    assert data["total_amount"] > 0

    p_ids = [p["product_id"] for p in data["purchases"]]
    assert p_ids.count("P00025442") == 2
    assert p_ids.count("P00110742") == 1

    # Verify rows in database
    history = repo.get_user_purchase_history(user_id)
    assert len(history) == initial_count + 3


def test_batch_purchase_partial_failure_rolls_back_everything(client, auth_header, repo):
    """Verify partial failure rolls back all items and records zero purchases."""
    headers, user_id = auth_header

    history_before = repo.get_user_purchase_history(user_id)
    count_before = len(history_before)

    # Payload with an invalid item that will fail during processing/pricing
    payload = {
        "items": [
            {
                "product_id": "P00025442",
                "product_category_1": 1,
                "quantity": 1,
            },
            {
                "product_id": "INVALID_PRODUCT_XYZ",
                "quantity": 0,  # Invalid quantity triggers 400
            },
        ]
    }

    response = client.post("/shopper/purchase/batch", json=payload, headers=headers)
    assert response.status_code in (400, 422, 500)

    # Assert NOTHING was committed
    history_after = repo.get_user_purchase_history(user_id)
    assert len(history_after) == count_before


def test_batch_purchase_database_error_rolls_back_everything(client, auth_header, repo):
    """Verify an unexpected exception during DB insert rolls back everything in the transaction."""
    headers, user_id = auth_header

    history_before = repo.get_user_purchase_history(user_id)
    count_before = len(history_before)

    records = [
        {
            "user_id": user_id,
            "product_id": "P00025442",
            "product_category_1": 1,
            "product_category_2": 6,
            "product_category_3": 14,
            "predicted_usd": 45.0,
            "model_used": "test_model",
        },
        {
            "user_id": 999999999,  # Non-existent user ID -> triggers ForeignKeyViolation
            "product_id": "P00110742",
            "product_category_1": 1,
            "product_category_2": 2,
            "product_category_3": 8,
            "predicted_usd": 50.0,
            "model_used": "test_model",
        },
    ]

    with pytest.raises(Exception):
        repo.record_purchases_batch(records)

    # Verify atomic rollback: 0 new purchases committed
    history_after = repo.get_user_purchase_history(user_id)
    assert len(history_after) == count_before


def test_single_item_purchase_still_works(client, auth_header, repo):
    """Verify single-item POST /shopper/purchase remains fully functional."""
    headers, user_id = auth_header

    payload = {
        "product_id": "P00025442",
        "product_category_1": 1,
        "product_category_2": 6,
        "product_category_3": 14,
    }

    response = client.post("/shopper/purchase", json=payload, headers=headers)
    assert response.status_code == 200, response.text
    data = response.json()
    assert data["product_id"] == "P00025442"
    assert data["predicted_usd"] > 0
    assert data["id"] > 0
