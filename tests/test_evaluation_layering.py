"""
Regression test for Extra: Evaluation Layering
- Validates that metric computation relocated to ml/models/metrics.py matches
  evaluation/ml/evaluate.py outputs bit-for-bit with identical inputs.
- Validates that ModelEvaluator imported from both locations behaves identically.
"""
import numpy as np
import pandas as pd
import pytest

from ml.models.metrics import (
    ModelEvaluator as CoreEvaluator,
    evaluate_holdout as core_holdout,
    evaluate_imputation as core_imputation,
    evaluate_hypothesis_welch as core_welch,
)
from evaluation.ml.evaluate import (
    ModelEvaluator as EvalEvaluator,
    evaluate_holdout as eval_holdout,
    evaluate_imputation as eval_imputation,
    evaluate_hypothesis_welch as eval_welch,
)


def test_evaluate_holdout_identical_outputs():
    """Verify evaluate_holdout produces identical outputs across both modules."""
    np.random.seed(42)
    y_true = np.random.uniform(1000, 20000, size=500)
    y_pred = y_true + np.random.normal(0, 500, size=500)

    res_core = core_holdout(y_true, y_pred)
    res_eval = eval_holdout(y_true, y_pred)

    assert res_core == res_eval
    assert "rmse" in res_core
    assert "r2" in res_core
    assert "mae" in res_core
    assert "mse" in res_core


def test_evaluate_imputation_identical_outputs():
    """Verify evaluate_imputation produces identical outputs across both modules."""
    np.random.seed(42)
    y_true = np.random.randint(1, 20, size=500)
    y_pred = y_true.copy()
    y_pred[:50] = np.random.randint(1, 20, size=50)

    res_core = core_imputation(y_true, y_pred, prefix="val")
    res_eval = eval_imputation(y_true, y_pred, prefix="val")

    assert res_core == res_eval


def test_evaluate_hypothesis_welch_identical_outputs():
    """Verify Welch's t-test produces identical outputs across both modules."""
    np.random.seed(42)
    sample_a = np.random.normal(10, 2, size=100)
    sample_b = np.random.normal(12, 3, size=100)

    res_core = core_welch(sample_a, sample_b)
    res_eval = eval_welch(sample_a, sample_b)

    assert res_core == res_eval


def test_model_evaluator_class_parity():
    """Verify ModelEvaluator instances from ml.models.metrics and evaluation.ml produce identical results."""
    eval_core = CoreEvaluator(n_splits=5, random_state=42)
    eval_pkg = EvalEvaluator(n_splits=5, random_state=42)

    y_true = np.array([100.0, 200.0, 300.0, 400.0])
    y_pred = np.array([105.0, 195.0, 302.0, 398.0])

    assert eval_core.evaluate_holdout(y_true, y_pred) == eval_pkg.evaluate_holdout(y_true, y_pred)
