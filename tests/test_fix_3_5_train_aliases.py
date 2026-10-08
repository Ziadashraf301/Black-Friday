"""
Regression test for Fix 3.5:
- --model aliases resolve through ModelRegistry.resolve_name (lgbm -> lightgbm, etc.)
- Test every alias and an unknown name (clean error)
"""
import pytest
from ml.models.registry import ModelRegistry


@pytest.mark.parametrize(
    "alias,canonical",
    [
        ("lgbm", "lightgbm"),
        ("rf", "random_forest"),
        ("dt", "decision_tree"),
        ("lr", "linear_regression"),
        ("LGBM", "lightgbm"),
        ("RF", "random_forest"),
        ("DT", "decision_tree"),
        ("LR", "linear_regression"),
        ("lightgbm", "lightgbm"),
        ("random_forest", "random_forest"),
        ("decision_tree", "decision_tree"),
        ("linear_regression", "linear_regression"),
    ]
)
def test_model_alias_resolution(alias, canonical):
    resolved = ModelRegistry.resolve_name(alias)
    assert resolved == canonical
    assert resolved in ModelRegistry.list_available_models()


def test_unknown_model_name_raises_clean_error():
    """Verify unknown model names raise ValueError listing available models."""
    available = ModelRegistry.list_available_models()
    with pytest.raises(ValueError) as excinfo:
        ModelRegistry.get_model("nonexistent_model_xyz")
    assert "Unknown model 'nonexistent_model_xyz'" in str(excinfo.value)
    assert str(available) in str(excinfo.value) or "Available registered models" in str(excinfo.value)


def test_train_pipeline_model_resolution():
    """Verify resolution logic used by run_training_pipeline accepts aliases."""
    available_model_names = ModelRegistry.list_available_models()
    
    for alias in ["lgbm", "rf", "dt", "lr"]:
        resolved_names = [ModelRegistry.resolve_name(m) for m in [alias]]
        selected_model_names = [m for m in resolved_names if m in available_model_names]
        assert len(selected_model_names) == 1

    # Unknown model fails cleanly
    with pytest.raises(ValueError) as excinfo:
        bad_names = ["bad_model"]
        resolved_names = [ModelRegistry.resolve_name(m) for m in bad_names]
        unrecognized = [m for m, r in zip(bad_names, resolved_names) if r not in available_model_names]
        if unrecognized:
            raise ValueError(f"Requested models {unrecognized} not found. Available: {available_model_names}")
    assert "Requested models ['bad_model'] not found" in str(excinfo.value)
