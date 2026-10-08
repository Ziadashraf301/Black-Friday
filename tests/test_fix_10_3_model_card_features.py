"""
Regression test for Fix 10.3:
- Verify generated Model Card markdown documents all 10 trained features.
- List is derived dynamically from ml.models.regression.ALL_FEATURES.
"""
import pytest
from ml.tracking.model_card import ModelCardGenerator
from ml.models.regression import ALL_FEATURES


def test_model_card_generator_documents_all_10_features(tmp_path):
    assert len(ALL_FEATURES) == 10

    metrics = {
        "test_rmse": 0.1234,
        "test_r2": 0.7200,
        "cv_mean_rmse": 0.1250,
        "cv_mean_r2": 0.7150,
    }
    fairness_metrics = {
        "gender_slices": {
            "M": {"sample_size": 100, "rmse": 0.12, "r2": 0.72},
            "F": {"sample_size": 50, "rmse": 0.13, "r2": 0.71},
        },
        "age_slices": {
            "26-35": {"sample_size": 80, "rmse": 0.12, "r2": 0.72},
        },
    }

    output_path = str(tmp_path / "test_model_card.md")
    generated_path = ModelCardGenerator.generate_model_card(
        model_name="lightgbm",
        metrics=metrics,
        fairness_metrics=fairness_metrics,
        output_filepath=output_path,
    )

    with open(generated_path, "r", encoding="utf-8") as f:
        content = f.read()

    # Check each of the 10 features is documented in the generated markdown
    for feat in ALL_FEATURES:
        assert f"`{feat}`" in content, f"Feature '{feat}' not found in generated Model Card."

    assert "Input Features:" in content
    assert "## 1. Model Overview" in content
    assert "## 2. Performance Summary" in content
    assert "## 3. Demographic Subgroup Fairness Evaluation" in content
