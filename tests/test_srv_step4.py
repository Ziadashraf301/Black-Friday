import json
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest
from sklearn.experimental import enable_iterative_imputer  # noqa: F401
from sklearn.impute import IterativeImputer
from sklearn.tree import DecisionTreeRegressor

from ml.models.onnx_exporter import ONNXExporter
from ml.models.regression import DecisionTreeModel, RandomForestModel, _parse_max_features
from ml.serving.predictor import ONNXPredictor


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
