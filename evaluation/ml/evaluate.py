"""
Evaluation entry point and reporting interface for ML models.
Imports core metric computations from ml.models.metrics and provides CLI interface.
"""
import numpy as np
from core.logging import get_logger
from ml.models.metrics import (
    ModelEvaluator,
    evaluate_holdout,
    cross_validate,
    evaluate_fairness,
    evaluate_imputation,
    evaluate_imputation_holdout,
    evaluate_hypothesis_welch,
)

logger = get_logger(__name__)

__all__ = [
    "ModelEvaluator",
    "evaluate_holdout",
    "cross_validate",
    "evaluate_fairness",
    "evaluate_imputation",
    "evaluate_imputation_holdout",
    "evaluate_hypothesis_welch",
]


if __name__ == "__main__":
    logger.info("Executing standalone model evaluation entry point...")
    evaluator = ModelEvaluator(n_splits=5)
    dummy_true = np.array([100.0, 150.0, 200.0, 250.0])
    dummy_pred = np.array([105.0, 145.0, 205.0, 240.0])
    sample_metrics = evaluator.evaluate_holdout(dummy_true, dummy_pred)
    print("ModelEvaluator baseline verification:")
    for k, v in sample_metrics.items():
        print(f"  {k}: {v}")
