import os
import json
import pandas as pd
from typing import Dict, Any

from core.db.repository import BlackFridayRepository
from ml.features.imputation import MissForestImputer
from ml.features.preprocessor import DataPreprocessor
from ml.features.data_contract import validate_cleaned_data
from ml.tracking.mlflow_tracker import MLflowTracker
from ml.visualization.visualizer import Visualizer
from ml.models.metrics import ModelEvaluator
from ml.models.onnx_exporter import ONNXExporter

from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


def run_preprocessing():
    """Extracts raw data, partitions train/test before imputation to prevent data leakage,
    performs MissForest imputation fitted on train with genuine OOB errors, evaluates on holdout test,
    tags outliers, normalizes target, persists split indicator to data warehouse,
    generates per-split and per-category diagnostic charts, logs audit telemetry,
    and registers the fitted imputer model in the MLflow Model Registry.
    """
    repo = BlackFridayRepository()
    tracker = MLflowTracker()
    preprocessor = DataPreprocessor()

    logger.info("DATA PREPROCESSING & FEATURE ENGINEERING PIPELINE")

    logger.info("Reading raw transactions from database warehouse...")
    raw_df = repo.get_raw_records_df()
    if raw_df.empty:
        raise ValueError("raw_black_friday table is empty. Please run ingestion pipeline first.")

    # 1. Reproducible Train / Test Split on Raw Records (Prevents Data Leakage)
    logger.info("Splitting raw data into Train (90%) and Test (10%) splits before imputation...")
    train_raw, test_raw = preprocessor.split_train_test(raw_df, test_size=0.10, exclude_outliers=False)
    logger.info(f"Raw split complete: Train={len(train_raw):,} records, Test={len(test_raw):,} records.")

    # 2. MissForest Iterative Imputation: Fit on Train Only, Transform Both
    imputer = MissForestImputer()
    logger.info("Fitting MissForest imputer on training split (computing OOB error)...")
    train_imputed = imputer.fit_transform(train_raw)

    logger.info("Transforming held-out test split using fitted MissForest imputer...")
    test_imputed = imputer.transform(test_raw)

    # 3. Evaluate Imputation Quality on Truly Unseen Holdout Ground-Truth Test Records
    logger.info("Evaluating MissForest imputation on held-out observed test records (NRMSE & PFC)...")
    imputation_eval = ModelEvaluator.evaluate_imputation_holdout(
        imputer=imputer,
        test_df=test_raw,
        sample_size=settings.IMPUTER_SAMPLE_SIZE or 50000,
        random_state=settings.RANDOM_SEED
    )

    # 4. Outlier Detection: Calculate Bounds on Train Only, Tag Both
    logger.info("Computing IQR outlier bounds from training split...")
    preprocessor.compute_outlier_bounds(train_imputed["purchase"])

    train_tagged = preprocessor.tag_outliers(train_imputed)
    test_tagged = preprocessor.tag_outliers(test_imputed)

    # 5. Target Normalization: Scale by Purchase_Max
    train_cleaned = preprocessor.normalize_target(train_tagged)
    test_cleaned = preprocessor.normalize_target(test_tagged)

    # 6. Add Split Indicator and Combine
    train_cleaned["split"] = "train"
    test_cleaned["split"] = "test"
    cleaned_df = pd.concat([train_cleaned, test_cleaned], ignore_index=True)

    # 7. Validate Cleaned Data against Pandera Schema Contract (including 'split')
    cleaned_df = validate_cleaned_data(cleaned_df)

    # 8. Generate Per-Split and Per-Category Diagnostic Visualizations
    logger.info("Generating preprocessing and imputation diagnostic charts...")
    os.makedirs("reports/preprocessing", exist_ok=True)
    charts = Visualizer.generate_preprocessing_charts(
        raw_df=raw_df,
        cleaned_df=cleaned_df,
        outlier_bounds=preprocessor.outlier_bounds,
        output_dir="reports/preprocessing"
    )

    imputation_charts = Visualizer.generate_imputation_charts(
        eval_data=imputation_eval,
        output_dir="reports/preprocessing"
    )
    charts.update(imputation_charts)

    # 9. Persist Cleaned Records with Split Indicator to Warehouse
    logger.info("Persisting cleaned data with train/test split indicator to database...")
    repo.truncate_cleaned_table()
    repo.insert_cleaned_batch(cleaned_df)

    try:
        from core.cache import cache_manager
        cache_manager.delete_pattern("analytics:*")
        logger.info("Invalidated analytics cache entries.")
    except Exception as e:
        logger.warning(f"Analytics cache invalidation warning: {e}")

    # 10. Export Fitted MissForest Imputer Model to Lightweight ONNX Format for Production Serving
    imputer_onnx_dir = os.path.join(settings.BASE_DIR, "models", "onnx", "imputer")
    logger.info(f"Exporting MissForest imputer to ONNX format at: {imputer_onnx_dir}...")
    ONNXExporter.export_imputer(imputer, output_dir=imputer_onnx_dir)
    logger.info(f"Persisted lightweight ONNX imputer artifacts to: {imputer_onnx_dir}")

    # 11. Assemble Full Audit Parameters and Metrics (Per Train and Test)
    train_outliers = int(train_cleaned["is_outlier"].sum())
    test_outliers = int(test_cleaned["is_outlier"].sum())
    total_outliers = train_outliers + test_outliers

    params: Dict[str, Any] = {
        **imputer.get_params(),
        "imputer_bootstrap": True,
        "imputer_oob_score": True,
        "split_strategy": "train_test_split_90_10",
        "train_records_count": len(train_cleaned),
        "test_records_count": len(test_cleaned),
        "outlier_method": "IQR_1.5",
        "outlier_lower_bound": round(preprocessor.outlier_bounds["lower"], 2),
        "outlier_upper_bound": round(preprocessor.outlier_bounds["upper"], 2),
        "outlier_threshold_constant": settings.OUTLIER_THRESHOLD,
        "normalization_method": "max_scaling",
        "normalization_purchase_max": preprocessor.purchase_max,
        "random_seed": settings.RANDOM_SEED
    }

    # Extract missing stats from imputer (calculated cleanly once)
    missing_before = imputer.stats.get("missing_before", {})

    metrics: Dict[str, float] = {
        "raw_records_count": float(len(raw_df)),
        "train_records_count": float(len(train_cleaned)),
        "test_records_count": float(len(test_cleaned)),
        "cleaned_records_count": float(len(cleaned_df)),
        "missing_product_category_2_raw": float(missing_before.get("product_category_2", 0)),
        "missing_product_category_3_raw": float(missing_before.get("product_category_3", 0)),
        "missing_product_category_2_imputed": 0.0,
        "missing_product_category_3_imputed": 0.0,
        "train_outliers_count": float(train_outliers),
        "train_outliers_percentage": round((train_outliers / len(train_cleaned)) * 100, 3),
        "test_outliers_count": float(test_outliers),
        "test_outliers_percentage": round((test_outliers / len(test_cleaned)) * 100, 3),
        "total_outliers_count": float(total_outliers),
        "total_outliers_percentage": round((total_outliers / len(cleaned_df)) * 100, 3),
        "train_raw_purchase_mean": round(float(train_cleaned["purchase"].mean()), 2),
        "test_raw_purchase_mean": round(float(test_cleaned["purchase"].mean()), 2),
        "train_cleaned_purchase_mean": round(float(train_cleaned[~train_cleaned["is_outlier"]]["purchase"].mean()), 2),
        "test_cleaned_purchase_mean": round(float(test_cleaned[~test_cleaned["is_outlier"]]["purchase"].mean()), 2),
        "train_normalized_purchase_mean": round(float(train_cleaned[~train_cleaned["is_outlier"]]["normalized_purchase"].mean()), 4),
        "test_normalized_purchase_mean": round(float(test_cleaned[~test_cleaned["is_outlier"]]["normalized_purchase"].mean()), 4),
        "imputation_duration_seconds": float(imputer.stats.get("imputation_duration_seconds", 0.0))
    }

    # Training OOB metrics from fitted estimators
    for col, oob_r2 in imputer.stats.get("oob_scores", {}).items():
        metrics[f"imputer_train_oob_{col}_r2"] = float(oob_r2)
    for col, oob_err in imputer.stats.get("oob_errors", {}).items():
        metrics[f"imputer_train_oob_{col}_error"] = float(oob_err)

    # Holdout Test Evaluation metrics (unseen test sample)
    eval_metrics = imputation_eval.get("metrics", {})
    for metric_key, metric_val in eval_metrics.items():
        metrics[f"imputation_{metric_key}"] = float(metric_val)

    # 12. Summary JSON audit file
    summary_path = os.path.join("reports", "preprocessing", "preprocessing_summary.json")
    with open(summary_path, "w", encoding="utf-8") as f:
        json.dump(
            {
                "params": params,
                "metrics": metrics,
                "imputer_stats": imputer.stats,
                "imputation_evaluation": eval_metrics
            },
            f,
            indent=2
        )

    artifacts = {
        **charts,
        "preprocessing_summary": summary_path
    }

    # 13. Log Preprocessing Run and Register/Promote Imputer Model in MLflow
    logger.info("Logging preprocessing run, metrics, charts, and registering ONNX imputer in MLflow...")
    run_id = tracker.log_preprocessing_run(
        params=params,
        metrics=metrics,
        tags={
            "pipeline_stage": "data_preprocessing",
            "contract_validation": "passed",
            "imputation_method": "MissForest",
            "outlier_strategy": "flagged_exclude_from_training",
            "split_ratio": "90/10",
            "model_format": "ONNX",
            "imputer_status": "PROMOTED_TO_PRODUCTION"
        },
        artifacts=artifacts,
        imputer_model_path=imputer_onnx_dir,
        register_imputer_name=settings.MLFLOW_IMPUTER_MODEL_NAME
    )

    logger.info(f"Preprocessing pipeline finished successfully. MLflow Run ID: {run_id}")
    return cleaned_df


if __name__ == "__main__":
    run_preprocessing()
