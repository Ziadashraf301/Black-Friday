"""
Regression test for Fix 9.4:
- Centralize INR_TO_USD currency conversion constant in settings
- Verify changing settings.INR_TO_USD proportionally changes model prediction prices
"""
import pytest
from unittest.mock import patch
import pandas as pd
from core.config import settings
from apps.api.services.model_service import model_service


@pytest.fixture(autouse=True)
def ensure_model_loaded():
    """Ensure champion model is loaded for testing."""
    model_service.load_models()


def test_predict_price_respects_inr_to_usd_setting():
    """Verify single-item predict_price scales inversely with settings.INR_TO_USD."""
    item_args = {
        "product_id": "P00025442",
        "cat1": 1,
        "cat2": 6,
        "cat3": 14,
        "gender": "M",
        "age": "26-35",
        "occupation": 4,
        "city_category": "B",
        "stay_in_current_city_years": "2",
        "marital_status": 0,
    }

    with patch.object(settings, "INR_TO_USD", 80.0):
        res_80 = model_service.predict_price(**item_args)

    with patch.object(settings, "INR_TO_USD", 160.0):
        res_160 = model_service.predict_price(**item_args)

    assert res_80["usd"] > 0
    assert res_160["usd"] > 0
    # Doubling exchange rate should halve the USD price (within rounding tolerance)
    assert abs(res_80["usd"] - 2 * res_160["usd"]) <= 0.05
    # Normalized model prediction should remain identical
    assert res_80["normalized"] == res_160["normalized"]


def test_predict_price_batch_matrix_respects_inr_to_usd_setting():
    """Verify vectorized 2D matrix prediction scales with settings.INR_TO_USD."""
    df = pd.DataFrame([{
        "product_id": "P00025442",
        "product_category_1": 1,
        "product_category_2": 6,
        "product_category_3": 14,
        "gender": "M",
        "age": "26-35",
        "occupation": 4,
        "city_category": "B",
        "stay_in_current_city_years": "2",
        "marital_status": 0,
    }])

    with patch.object(settings, "INR_TO_USD", 80.0):
        batch_80 = model_service.predict_price_batch_matrix(df)

    with patch.object(settings, "INR_TO_USD", 100.0):
        batch_100 = model_service.predict_price_batch_matrix(df)

    p_80 = batch_80.iloc[0]["usd"]
    p_100 = batch_100.iloc[0]["usd"]

    assert p_80 > p_100
    assert abs(p_80 * 80.0 - p_100 * 100.0) <= 0.5
