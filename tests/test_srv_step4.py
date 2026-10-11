import pandas as pd
import pytest

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
