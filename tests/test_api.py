import pytest
from unittest.mock import MagicMock
from fastapi.testclient import TestClient
from apps.api.main import app
from apps.api.dependencies import get_repository
from apps.api.services.model_service import model_service
from core.security import hash_password

@pytest.fixture
def mock_repo():
    repo = MagicMock()
    repo.get_eda_summary.return_value = {
        "total_orders": 550068,
        "total_users": 5891,
        "total_products": 3631,
        "avg_order_value": 9263.97,
        "total_revenue": 5095812740.0
    }
    repo.get_dimension_distribution.return_value = [
        {"category": "M", "order_count": 414259, "avg_purchase": 9437.53, "total_purchase": 3909590000.0},
        {"category": "F", "order_count": 135809, "avg_purchase": 8734.57, "total_purchase": 1186222740.0},
    ]
    repo.get_dimension_stats.return_value = {
        "male": {"count": 414259, "mean": 9437.53, "std": 5000.0, "var": 25000000.0},
        "female": {"count": 135809, "mean": 8734.57, "std": 4800.0, "var": 23040000.0}
    }
    repo.get_all_personas_summary.return_value = [
        {"cluster_id": 1, "cluster_persona": "Single females <= 50", "recommended_action": "Lifestyle deals."},
        {"cluster_id": 2, "cluster_persona": "Single males <= 50", "recommended_action": "Tech deals."}
    ]
    repo.get_top_network_products.return_value = [
        {
            "product_id": "P00110742",
            "order_count": 1612,
            "pagerank_score": 0.175,
            "hub_score": 0.884,
            "authority_score": 1.0,
            "top_associated_product": "P00025442",
            "highest_lift_rule": 1.58
        }
    ]
    repo.get_product_recommendations.return_value = {
        "product_id": "P00110742",
        "order_count": 1612,
        "pagerank_score": 0.175,
        "hub_score": 0.884,
        "authority_score": 1.0,
        "top_associated_product": "P00025442",
        "highest_lift_rule": 1.58,
        "top_bundle_recommendations": [{"product_id": "P00025442", "confidence": 0.48, "lift": 1.58}],
        "item2vec_recommendations": '["P00057642"]'
    }
    repo.get_product_categories.return_value = {
        "product_category_1": 3,
        "product_category_2": 4,
        "product_category_3": 12
    }
    repo.get_user_by_email.return_value = None
    repo.create_user.return_value = {
        "user_id": 1,
        "name": "Jane Doe",
        "email": "jane@example.com",
        "password_hash": hash_password("secretpass"),
        "cluster_id": 1,
        "cluster_persona": "Single females <= 50",
        "gender": "F",
        "age": "26-35",
        "city_category": "A",
        "marital_status": 0,
        "occupation": 1,
        "stay_in_current_city_years": "2",
        "recommended_action": "Targeted lifestyle promotions"
    }
    repo.get_user_by_id.return_value = {
        "user_id": 1,
        "name": "Jane Doe",
        "email": "jane@example.com",
        "cluster_id": 1,
        "cluster_persona": "Single females <= 50",
        "gender": "F",
        "age": "26-35",
        "city_category": "A",
        "marital_status": 0,
        "occupation": 1,
        "stay_in_current_city_years": "2",
        "recommended_action": "Targeted lifestyle promotions"
    }
    repo.record_purchase.return_value = {
        "id": 101,
        "user_id": 1,
        "product_id": "P00110742",
        "product_category_1": 3,
        "product_category_2": 4,
        "product_category_3": 12,
        "predicted_usd": 9437.53,
        "model_used": "random_forest@champion",
        "purchased_at": "2026-09-26 12:00:00"
    }
    repo.get_user_purchase_history.return_value = [
        {
            "id": 101,
            "product_id": "P00110742",
            "product_category_1": 3,
            "product_category_2": 4,
            "product_category_3": 12,
            "predicted_usd": 9437.53,
            "model_used": "random_forest@champion",
            "purchased_at": "2026-09-26 12:00:00"
        }
    ]
    return repo

@pytest.fixture
def client(mock_repo):
    app.dependency_overrides[get_repository] = lambda: mock_repo
    # Mock model_service predict_price
    orig_predict = model_service.predict_price
    model_service.predict_price = MagicMock(return_value={
        "usd": 9437.53,
        "normalized": 0.441,
        "model_used": "random_forest (local)"
    })
    yield TestClient(app)
    app.dependency_overrides.clear()
    model_service.predict_price = orig_predict

def test_health_check(client):
    response = client.get("/health")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "healthy"
    assert data["service"] == "Black-Friday-v2"

def test_analytics_summary(client):
    response = client.get("/analytics/summary")
    assert response.status_code == 200
    data = response.json()
    assert data["total_orders"] == 550068
    assert data["total_revenue"] == 5095812740.0

def test_eda_with_stats_gender(client):
    response = client.get("/analytics/eda-with-stats/gender")
    assert response.status_code == 200
    data = response.json()
    assert data["dimension"] == "gender"
    assert "test_statistic" in data
    assert "is_significant" in data
    assert len(data["categories"]) > 0

def test_auth_signup_and_me(client, mock_repo):
    signup_payload = {
        "name": "Jane Doe",
        "email": "jane@example.com",
        "password": "securepassword",
        "gender": "F",
        "age": "26-35",
        "city_category": "A",
        "marital_status": 0,
        "occupation": 1,
        "stay_in_current_city_years": "2"
    }
    res = client.post("/auth/signup", json=signup_payload)
    assert res.status_code == 201
    auth_data = res.json()
    assert "access_token" in auth_data
    token = auth_data["access_token"]

    # Test /auth/me with Bearer token
    headers = {"Authorization": f"Bearer {token}"}
    res_me = client.get("/auth/me", headers=headers)
    assert res_me.status_code == 200
    user_me = res_me.json()
    assert user_me["name"] == "Jane Doe"
    assert user_me["cluster_id"] == 1

def test_shopper_catalog_and_browse(client):
    res_cat = client.get("/shopper/catalog?limit=5")
    assert res_cat.status_code == 200
    cat_items = res_cat.json()
    assert len(cat_items) == 1
    assert cat_items[0]["product_id"] == "P00110742"

    res_browse = client.get("/shopper/browse/P00110742")
    assert res_browse.status_code == 200
    browse_data = res_browse.json()
    assert browse_data["product_id"] == "P00110742"
    assert "P00025442" in browse_data["apriori_bundles"]

def test_shopper_predict_and_purchase_flow(client):
    # Register/get token
    signup_payload = {
        "name": "Jane Doe",
        "email": "jane@example.com",
        "password": "securepassword",
        "gender": "F",
        "age": "26-35",
        "city_category": "A",
        "marital_status": 0,
        "occupation": 1,
        "stay_in_current_city_years": "2"
    }
    signup_res = client.post("/auth/signup", json=signup_payload)
    token = signup_res.json()["access_token"]
    headers = {"Authorization": f"Bearer {token}"}

    # Predict price
    predict_payload = {
        "product_id": "P00110742",
        "product_category_1": 3,
        "product_category_2": 4,
        "product_category_3": 12
    }
    res_pred = client.post("/shopper/predict-price", json=predict_payload, headers=headers)
    assert res_pred.status_code == 200
    pred_data = res_pred.json()
    assert pred_data["predicted_usd"] > 0

    # Purchase
    res_purch = client.post("/shopper/purchase", json=predict_payload, headers=headers)
    assert res_purch.status_code == 200
    purch_data = res_purch.json()
    assert purch_data["id"] == 101

    # History
    res_hist = client.get("/shopper/history", headers=headers)
    assert res_hist.status_code == 200
    hist_data = res_hist.json()
    assert hist_data["total_purchases"] == 1
