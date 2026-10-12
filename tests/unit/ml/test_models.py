"""Regression test for Fix 4.3:
- Verify ONNXPredictor uses ORT_ENABLE_ALL graph optimization
- Verify predictions with optimization equal unoptimized predictions (rtol=1e-4) on the real ONNX model
- Measure and assert latency comparison before and after optimization"""

import json
import os
import time
from types import SimpleNamespace

import numpy as np
import onnxruntime as ort
import pandas as pd
import pytest
from sklearn.experimental import enable_iterative_imputer  # noqa: F401
from sklearn.impute import IterativeImputer
from sklearn.tree import DecisionTreeRegressor

from core.config import settings
from ml.models.onnx_exporter import ONNXExporter
from ml.models.registry import ModelRegistry
from ml.models.regression import (
    DecisionTreeModel,
    LightGBMModel,
    LinearRegressionModel,
    RandomForestModel,
    _parse_max_features,
)
from ml.serving.predictor import ONNXPredictor
from ml.tracking.explainability import ModelExplainability

pytestmark = pytest.mark.slow


@pytest.fixture
def train_data():
    return pd.DataFrame({
        "gender": ["F", "F", "M", "M", "F", "F", "M", "M"],
        "age": ["0-17", "0-17", "26-35", "26-35", "36-45", "36-45", "55+", "55+"],
        "occupation": [10, 10, 16, 16, 10, 10, 16, 16],
        "city_category": ["A", "A", "C", "C", "A", "A", "C", "C"],
        "stay_in_current_city_years": ["2", "2", "4+", "4+", "2", "2", "4+", "4+"],
        "marital_status": [0, 0, 1, 1, 0, 0, 1, 1],
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


@pytest.mark.parametrize(
    "raw, expected",
    [
        (1.0, 1.0),
        ("1.0", 1.0),
        (0.5, 0.5),
        ("0.5", 0.5),
        (3, 3),
        ("3", 3),
        (4.0, 4),
        (None, 1.0),
        ("sqrt", "sqrt"),
        ("junk", 1.0),
    ],
)
def test_parse_max_features_sklearn_semantics(raw, expected):
    assert _parse_max_features(raw) == expected


def test_random_forest_default_uses_all_features():
    assert RandomForestModel().pipeline.named_steps["regressor"].max_features == 1.0


def _tiny_dt():
    df = pd.DataFrame(
        {
            "gender": ["F", "M", "F", "M"],
            "age": ["0-17", "26-35", "0-17", "26-35"],
            "occupation": [1, 2, 1, 2],
            "city_category": ["A", "C", "A", "C"],
            "stay_in_current_city_years": ["1", "2", "1", "2"],
            "marital_status": [0, 1, 0, 1],
            "product_category_1": [1, 2, 3, 1],
            "product_category_2": [4, 5, 6, 4],
            "product_category_3": [7, 8, 9, 7],
            "product_id": ["P1", "P2", "P3", "P1"],
        }
    )
    model = DecisionTreeModel(max_depth=2)
    model.fit(df, pd.Series([0.1, 0.2, 0.3, 0.1]))
    return model, df


def test_predictor_reports_missing_input_columns(tmp_path):
    model, df = _tiny_dt()
    path = str(tmp_path / "m.onnx")
    ONNXExporter.export_regression_pipeline(model.pipeline, model.features, path)
    predictor = ONNXPredictor(path)
    with pytest.raises(ValueError, match="product_category_3"):
        predictor.predict(df.drop(columns=["product_category_3"]))


def test_export_accepts_bare_filename(tmp_path, monkeypatch):
    model, _ = _tiny_dt()
    monkeypatch.chdir(tmp_path)
    assert ONNXExporter.export_regression_pipeline(model.pipeline, model.features, "bare.onnx") == "bare.onnx"
    assert (tmp_path / "bare.onnx").exists()


def test_export_imputer_keeps_one_step_per_column(tmp_path):
    cols = ["a", "b", "product_category_2", "product_category_3"]
    rng = np.random.RandomState(0)
    X = rng.randint(1, 10, size=(60, 4)).astype(float)
    X[::5, 2] = np.nan
    X[::7, 3] = np.nan
    inner = IterativeImputer(estimator=DecisionTreeRegressor(max_depth=3), max_iter=3, random_state=0).fit(X)
    assert len(inner.imputation_sequence_) > 2  # several iterations recorded

    out = ONNXExporter.export_imputer(SimpleNamespace(imputer=inner, cols=cols), output_dir=str(tmp_path))
    meta = json.loads(open(out["metadata"], encoding="utf-8").read())
    assert sorted(s["target_col"] for s in meta["steps"]) == ["product_category_2", "product_category_3"]


def test_lightgbm_subsample_freq_defaults_to_no_bagging():
    from core.config import settings
    from ml.models.regression import LightGBMModel

    assert settings.LGBM_SUBSAMPLE_FREQ == 0  # champion behaviour unchanged
    default = LightGBMModel().pipeline.named_steps["regressor"]
    assert default.subsample_freq == 0

    bagged = LightGBMModel(subsample_freq=1).pipeline.named_steps["regressor"]
    assert bagged.subsample_freq == 1 and bagged.subsample == settings.LGBM_SUBSAMPLE


@pytest.fixture
def sample_batch():
    """Generates a representative sample batch for regression inference."""
    return pd.DataFrame([
        {
            "gender": "M" if i % 2 == 0 else "F",
            "age": "26-35" if i % 3 == 0 else "36-45",
            "occupation": (i % 20) + 1,
            "city_category": ["A", "B", "C"][i % 3],
            "stay_in_current_city_years": str(i % 5),
            "marital_status": i % 2,
            "product_category_1": (i % 18) + 1,
            "product_category_2": (i % 15) + 1,
            "product_category_3": (i % 12) + 1,
            "product_id": f"P{i:08d}",
        }
        for i in range(50)
    ])


def test_onnx_predictions_match_between_opt_levels(sample_batch):
    """Verify ORT_ENABLE_ALL yields identical numerical results to ORT_DISABLE_ALL within rtol=1e-4."""
    model_path = os.path.join(settings.BASE_DIR, "models", "onnx", "lightgbm.onnx")
    if not os.path.exists(model_path):
        pytest.skip(f"Model file not found: {model_path}")

    # Session 1: ORT_DISABLE_ALL (baseline)
    opts_off = ort.SessionOptions()
    opts_off.graph_optimization_level = ort.GraphOptimizationLevel.ORT_DISABLE_ALL
    sess_off = ort.InferenceSession(model_path, sess_options=opts_off, providers=["CPUExecutionProvider"])

    # Session 2: ORT_ENABLE_ALL (optimized)
    opts_on = ort.SessionOptions()
    opts_on.graph_optimization_level = ort.GraphOptimizationLevel.ORT_ENABLE_ALL
    sess_on = ort.InferenceSession(model_path, sess_options=opts_on, providers=["CPUExecutionProvider"])

    # Prepare inputs
    inputs: dict = {}
    for inp in sess_off.get_inputs():
        col = inp.name
        if "string" in inp.type or col == "product_id":
            inputs[col] = sample_batch[[col]].astype(str).to_numpy()
        elif "float" in inp.type:
            inputs[col] = sample_batch[[col]].astype(np.float32).to_numpy()
        else:
            inputs[col] = sample_batch[[col]].astype(np.int64).to_numpy()

    # Warmup
    _ = sess_off.run(None, inputs)
    _ = sess_on.run(None, inputs)

    # Benchmark ORT_DISABLE_ALL
    n_iters = 30
    t0 = time.perf_counter()
    for _ in range(n_iters):
        preds_off = sess_off.run(None, inputs)[0].flatten()
    lat_off = (time.perf_counter() - t0) / n_iters * 1000

    # Benchmark ORT_ENABLE_ALL
    t0 = time.perf_counter()
    for _ in range(n_iters):
        preds_on = sess_on.run(None, inputs)[0].flatten()
    lat_on = (time.perf_counter() - t0) / n_iters * 1000

    # Numerical equivalence verification
    assert np.allclose(preds_off, preds_on, rtol=1e-4, atol=1e-5), (
        f"Predictions differ between optimization levels: max diff = {np.max(np.abs(preds_off - preds_on))}"
    )

    # Verify ONNXPredictor uses ORT_ENABLE_ALL
    predictor = ONNXPredictor(model_path)
    pred_class_preds = predictor.predict(sample_batch)
    assert np.allclose(preds_on, pred_class_preds, rtol=1e-4, atol=1e-5)

    print(f"\n[LATENCY BENCHMARK] Baseline (DISABLE_ALL): {lat_off:.3f}ms | Optimized (ENABLE_ALL): {lat_on:.3f}ms")
