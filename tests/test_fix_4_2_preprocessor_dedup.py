"""
Regression test for Fix 4.2 (Deduplication half):
- Extract duplicated ColumnTransformer into create_tree_preprocessor() in ml/models/regression.py.
- Verify DecisionTreeModel, RandomForestModel, and LightGBMModel use the factory.
- Verify transformer structure and feature groupings are identical across models.
"""
import pytest
from sklearn.compose import ColumnTransformer
from ml.models.regression import (
    create_tree_preprocessor,
    DecisionTreeModel,
    RandomForestModel,
    LightGBMModel,
    NUMERIC_FEATS,
    CATEGORICAL_FEATS,
)


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
