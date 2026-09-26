import pytest
import os
import pandas as pd
import numpy as np
from ml.models.regression import (
    LinearRegressionModel,
    DecisionTreeModel,
    RandomForestModel,
    LightGBMModel
)
from ml.models.registry import ModelRegistry
from ml.models.onnx_exporter import ONNXExporter
from apps.api.serving.predictor import ONNXPredictor
from ml.tracking.explainability import ModelExplainability
from core.config import settings


@pytest.fixture
def train_data():
    return pd.DataFrame({
        "product_category_1": [1, 2, 3, 1, 2, 3, 1, 2],
        "product_category_2": [4, 5, 6, 4, 5, 6, 4, 5],
        "product_category_3": [7, 8, 9, 7, 8, 9, 7, 8],
        "product_id": ["P1", "P2", "P3", "P1", "P2", "P3", "P1", "P2"],
        "target": [0.4, 0.5, 0.6, 0.42, 0.49, 0.61, 0.39, 0.51]
    })


def test_linear_regression_model(train_data):
    model = LinearRegressionModel()
    model.fit(train_data, train_data["target"])
    preds = model.predict(train_data)
    assert len(preds) == len(train_data)
    assert np.all(preds >= 0)


def test_decision_tree_model(train_data):
    model = DecisionTreeModel()
    model.fit(train_data, train_data["target"])
    preds = model.predict(train_data)
    assert len(preds) == len(train_data)


def test_random_forest_model(train_data):
    model = RandomForestModel(n_estimators=10, max_features=2)
    model.fit(train_data, train_data["target"])
    preds = model.predict(train_data)
    assert len(preds) == len(train_data)


def test_lightgbm_model(train_data):
    model = LightGBMModel(n_estimators=10, num_leaves=7)
    model.fit(train_data, train_data["target"])
    preds = model.predict(train_data)
    assert len(preds) == len(train_data)


def test_model_registry_dynamic_settings():
    dt_params = ModelRegistry.get_default_params("decision_tree")
    assert dt_params["max_depth"] == settings.DT_MAX_DEPTH
    assert dt_params["min_samples_leaf"] == settings.DT_MIN_SAMPLES_LEAF

    rf_params = ModelRegistry.get_default_params("random_forest")
    assert rf_params["n_estimators"] == settings.RF_N_ESTIMATORS
    assert rf_params["max_depth"] == settings.RF_MAX_DEPTH

    lgbm_params = ModelRegistry.get_default_params("lightgbm")
    assert lgbm_params["n_estimators"] == settings.LGBM_N_ESTIMATORS
    assert lgbm_params["learning_rate"] == settings.LGBM_LEARNING_RATE

    # Model instantiation with overrides
    model = ModelRegistry.get_model("decision_tree", max_depth=8)
    assert model.max_depth == 8


def test_model_explainability_linear_and_tree(train_data):
    # Tree model SHAP
    dt_model = DecisionTreeModel(max_depth=4)
    dt_model.fit(train_data, train_data["target"])
    explainer_tree = ModelExplainability(dt_model.pipeline, background_sample=train_data.head(4))
    shap_vals_tree, _ = explainer_tree.explain(train_data.head(4))
    assert shap_vals_tree is not None

    # Linear model SHAP
    lr_model = LinearRegressionModel()
    lr_model.fit(train_data, train_data["target"])
    explainer_linear = ModelExplainability(lr_model.pipeline, background_sample=train_data.head(4))
    shap_vals_linear, _ = explainer_linear.explain(train_data.head(4))
    assert shap_vals_linear is not None


def test_serving_onnx_predictor(tmp_path, train_data):
    model = DecisionTreeModel(max_depth=4)
    model.fit(train_data, train_data["target"])

    onnx_file = str(tmp_path / "dt_model.onnx")
    ONNXExporter.export_regression_pipeline(
        pipeline=model.pipeline,
        feature_names=model.features,
        output_filepath=onnx_file
    )

    predictor = ONNXPredictor(onnx_model_path=onnx_file)
    preds = predictor.predict(train_data[model.features])
    assert len(preds) == len(train_data)
    assert np.all(np.isfinite(preds))
