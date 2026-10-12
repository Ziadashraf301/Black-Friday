"""Regression test for Fix 4.2 (Deduplication half):
- Extract duplicated ColumnTransformer into create_tree_preprocessor() in ml/models/regression.py.
- Verify DecisionTreeModel, RandomForestModel, and LightGBMModel use the factory.
- Verify transformer structure and feature groupings are identical across models."""

import os

import numpy as np
import pandas as pd
import pytest
from sklearn.compose import ColumnTransformer

from ml.features.customer_features import CustomerFeatureExtractor
from ml.features.preprocessor import DataPreprocessor
from ml.models.regression import (
    CATEGORICAL_FEATS,
    NUMERIC_FEATS,
    DecisionTreeModel,
    LightGBMModel,
    RandomForestModel,
    create_tree_preprocessor,
)

pytestmark = pytest.mark.slow


@pytest.fixture
def sample_transactions():
    return pd.DataFrame(
        {
            "user_id": [1000001, 1000001, 1000002, 1000002],
            "product_id": ["P0001", "P0002", "P0001", "P0003"],
            "gender": ["F", "F", "M", "M"],
            "age": ["0-17", "0-17", "55+", "55+"],
            "occupation": [10, 10, 16, 16],
            "city_category": ["A", "A", "C", "C"],
            "stay_in_current_city_years": ["2", "2", "4+", "4+"],
            "marital_status": [0, 0, 1, 1],
            "product_category_1": [3, 1, 8, 8],
            "product_category_2": [4, 6, 2, 2],
            "product_category_3": [12, 14, 5, 5],
            "purchase": [8370.0, 15200.0, 7969.0, 22500.0],
        }
    )


def test_preprocessor_outlier_bounds(sample_transactions):
    preprocessor = DataPreprocessor()
    preprocessor.compute_outlier_bounds(sample_transactions["purchase"])
    tagged = preprocessor.tag_outliers(sample_transactions)
    assert "is_outlier" in tagged.columns
    assert tagged["is_outlier"].dtype == bool


def test_preprocessor_normalization(sample_transactions):
    preprocessor = DataPreprocessor(purchase_max=21399.0)
    norm_df = preprocessor.normalize_target(sample_transactions)
    assert "normalized_purchase" in norm_df.columns
    assert norm_df["normalized_purchase"].iloc[0] == pytest.approx(8370.0 / 21399.0)


def test_customer_feature_extractor(sample_transactions):
    extractor = CustomerFeatureExtractor()
    features = extractor.extract_features(sample_transactions)
    assert len(features) == 2  # Two unique users
    assert "lifetime_value" in features.columns
    assert "average_order_value" in features.columns
    assert "frequency" in features.columns
    assert "age_binned" in features.columns
    # Check user 1000001 lifetime value
    u1 = features[features["user_id"] == 1000001].iloc[0]
    assert u1["lifetime_value"] == 8370.0 + 15200.0
    assert u1["frequency"] == 2
    assert u1["age_binned"] == "<=50"


def test_cleaned_data_contract_with_split(sample_transactions):
    from ml.features.data_contract import validate_cleaned_data

    preprocessor = DataPreprocessor()
    preprocessor.compute_outlier_bounds(sample_transactions["purchase"])
    tagged = preprocessor.tag_outliers(sample_transactions)
    cleaned = preprocessor.normalize_target(tagged)
    cleaned["split"] = "train"

    # Valid schema passes
    validated = validate_cleaned_data(cleaned)
    assert "split" in validated.columns
    assert validated["split"].iloc[0] == "train"

    # Invalid split value fails validation
    from pandera.errors import SchemaError

    cleaned_invalid = cleaned.copy()
    cleaned_invalid["split"] = "invalid_split"
    with pytest.raises(SchemaError):
        validate_cleaned_data(cleaned_invalid)


def test_onnx_imputer_export_and_inference(tmp_path):
    from ml.features.imputation import MissForestImputer
    from ml.models.onnx_exporter import ONNXExporter, ONNXMissForestImputer

    # Create synthetic dataset with missing values
    np.random.seed(42)
    n = 200
    df = pd.DataFrame(
        {
            "product_category_1": np.random.randint(1, 20, size=n),
            "product_category_2": np.where(np.random.rand(n) < 0.25, np.nan, np.random.randint(2, 18, size=n)),
            "product_category_3": np.where(np.random.rand(n) < 0.40, np.nan, np.random.randint(3, 18, size=n)),
            "purchase": np.random.uniform(2000, 20000, size=n),
        }
    )

    imputer = MissForestImputer(n_estimators=5, max_iter=2, max_depth=6, min_samples_leaf=3)
    fitted_df = imputer.fit_transform(df)
    assert fitted_df["product_category_2"].isna().sum() == 0
    assert fitted_df["product_category_3"].isna().sum() == 0

    # Export to ONNX in temp dir
    export_dir = str(tmp_path / "onnx_imputer")
    saved_files = ONNXExporter.export_imputer(imputer, output_dir=export_dir)
    assert os.path.exists(os.path.join(export_dir, "imputer_product_category_2.onnx"))
    assert os.path.exists(os.path.join(export_dir, "imputer_product_category_3.onnx"))
    assert os.path.exists(os.path.join(export_dir, "imputer_metadata.json"))

    # Validate ONNX runtime inference
    onnx_imputer = ONNXMissForestImputer(export_dir)
    onnx_transformed = onnx_imputer.transform(df)
    assert onnx_transformed["product_category_2"].isna().sum() == 0
    assert onnx_transformed["product_category_3"].isna().sum() == 0


def test_create_tree_preprocessor_returns_columntransformer():
    """Verify factory returns properly configured ColumnTransformer."""
    preproc = create_tree_preprocessor()
    assert isinstance(preproc, ColumnTransformer)
    transformer_names = [t[0] for t in preproc.transformers]
    assert "num" in transformer_names
    assert "cat" in transformer_names


def test_tree_models_use_identical_preprocessor_configuration():
    """Verify DT, RF, and LightGBM have identical transformer specifications."""
    dt = DecisionTreeModel()
    rf = RandomForestModel()
    lgbm = LightGBMModel()

    factory_preproc = create_tree_preprocessor()

    for model_instance in [dt, rf, lgbm]:
        model_preproc = model_instance.preprocessor
        assert isinstance(model_preproc, ColumnTransformer)

        # Check transformers list matches factory specification
        assert len(model_preproc.transformers) == len(factory_preproc.transformers)
        for t_model, t_fac in zip(model_preproc.transformers, factory_preproc.transformers):
            assert t_model[0] == t_fac[0]  # name matches ("num", "cat")
            assert t_model[2] == t_fac[2]  # feature list matches
            assert t_model[2] == NUMERIC_FEATS if t_model[0] == "num" else CATEGORICAL_FEATS + ["product_id"]
