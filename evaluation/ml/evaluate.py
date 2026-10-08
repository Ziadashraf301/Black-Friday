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
    import os
    import pandas as pd
    from core.config import settings
    from ml.serving.predictor import ONNXPredictor

    logger.info("Executing standalone model evaluation entry point on real holdout...")
    evaluator = ModelEvaluator(n_splits=5)

    # 1. Locate real holdout test data
    test_csv_path = settings.BASE_DIR / "data" / "test.csv"
    onnx_model_path = settings.BASE_DIR / "models" / "onnx" / "lightgbm.onnx"

    if test_csv_path.exists() and onnx_model_path.exists():
        logger.info(f"Loading real holdout dataset from {test_csv_path}...")
        test_df = pd.read_csv(test_csv_path)

        # Standardize target if purchase exists
        target_col = "normalized_purchase" if "normalized_purchase" in test_df.columns else "purchase"
        if target_col in test_df.columns:
            y_true = test_df[target_col].to_numpy()
        else:
            # If test.csv lacks target, check warehouse repository or fallback to train holdout
            y_true = None

        if y_true is not None:
            logger.info(f"Loading champion model from {onnx_model_path}...")
            predictor = ONNXPredictor(str(onnx_model_path))
            y_pred = predictor.predict(test_df)

            metrics = evaluator.evaluate_holdout(y_true, y_pred)
            print("==================================================")
            print("Real Holdout Evaluation Metrics (Champion LightGBM):")
            for k, v in metrics.items():
                print(f"  {k}: {v}")
            print("==================================================")
        else:
            logger.warning("Target column not found in test.csv. Evaluating sample from warehouse repository...")
            from core.db.repository import BlackFridayRepository
            repo = BlackFridayRepository()
            eval_df = repo.warehouse.get_cleaned_transactions(limit=10000)
            if not eval_df.empty and "normalized_purchase" in eval_df.columns:
                predictor = ONNXPredictor(str(onnx_model_path))
                y_true = eval_df["normalized_purchase"].to_numpy()
                y_pred = predictor.predict(eval_df)
                metrics = evaluator.evaluate_holdout(y_true, y_pred)
                print("==================================================")
                print("Warehouse Holdout Evaluation Metrics (Champion LightGBM):")
                for k, v in metrics.items():
                    print(f"  {k}: {v}")
                print("==================================================")
    else:
        logger.warning("Real holdout data or ONNX model artifact missing. Running baseline verification fallback...")
        dummy_true = np.array([100.0, 150.0, 200.0, 250.0])
        dummy_pred = np.array([105.0, 145.0, 205.0, 240.0])
        sample_metrics = evaluator.evaluate_holdout(dummy_true, dummy_pred)
        print("ModelEvaluator baseline verification:")
        for k, v in sample_metrics.items():
            print(f"  {k}: {v}")

