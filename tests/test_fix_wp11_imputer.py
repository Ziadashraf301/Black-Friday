import numpy as np
import pandas as pd
import pytest

from ml.features.imputation import MissForestImputer
from ml.serving.imputer import ONNXMissForestImputer


def test_missforest_no_purchase_leakage():
    # Verify purchase is not in FEATURE_COLS
    assert "purchase" not in MissForestImputer.FEATURE_COLS
    assert "normalized_purchase" not in MissForestImputer.FEATURE_COLS


def test_missforest_transform_without_purchase():
    df = pd.DataFrame({
        "gender": ["M", "F", "M"],
        "age": ["26-35", "18-25", "36-45"],
        "occupation": [1, 2, 3],
        "city_category": ["A", "B", "C"],
        "stay_in_current_city_years": [1, 2, 3],
        "marital_status": [0, 1, 0],
        "product_category_1": [1, 2, 3],
        "product_category_2": [np.nan, 5, np.nan],
        "product_category_3": [np.nan, np.nan, 8],
    })
    
    imputer = MissForestImputer(max_iter=2, n_estimators=5, random_state=42)
    imputed_df = imputer.fit_transform(df)

    assert "product_category_2" in imputed_df.columns
    assert "product_category_3" in imputed_df.columns
    assert imputed_df["product_category_2"].isna().sum() == 0
    assert imputed_df["product_category_3"].isna().sum() == 0
    
    # Check clipping within [1, 20]
    assert (imputed_df["product_category_2"] >= 1).all()
    assert (imputed_df["product_category_2"] <= 20).all()
    assert (imputed_df["product_category_3"] >= 1).all()
    assert (imputed_df["product_category_3"] <= 20).all()


def test_serving_onnx_imputer_clips_and_handles_missing_categories():
    import os
    model_dir = "models/onnx/imputer"
    if not os.path.exists(model_dir):
        pytest.skip("ONNX imputer models not found in local models/onnx/imputer")

    imputer = ONNXMissForestImputer(model_dir=model_dir)

    sample_df = pd.DataFrame([{
        "gender": "M",
        "age": "26-35",
        "occupation": 4,
        "city_category": "B",
        "stay_in_current_city_years": 2,
        "marital_status": 1,
        "product_category_1": 1,
    }])
    result_df = imputer.transform(sample_df)
    
    assert "product_category_2" in result_df.columns
    assert "product_category_3" in result_df.columns
    cat2 = result_df["product_category_2"].iloc[0]
    cat3 = result_df["product_category_3"].iloc[0]
    assert 1 <= cat2 <= 20
    assert 1 <= cat3 <= 20
