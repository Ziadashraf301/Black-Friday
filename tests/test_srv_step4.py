import pytest

from ml.models.regression import RandomForestModel, _parse_max_features


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
