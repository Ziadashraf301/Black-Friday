"""
Regression test for Fix 2.5:
- Verify ModelRegistry is exported from ml.models root package.
"""
import pytest


def test_model_registry_imported_from_ml_models():
    """Verify ModelRegistry is importable directly from ml.models."""
    from ml.models import ModelRegistry

    assert ModelRegistry is not None
    assert hasattr(ModelRegistry, "get_model")
    assert hasattr(ModelRegistry, "resolve_name")
    assert hasattr(ModelRegistry, "list_available_models")


def test_model_registry_in_ml_models_all():
    """Verify ModelRegistry is present in __all__ of ml.models."""
    import ml.models

    assert "ModelRegistry" in ml.models.__all__
