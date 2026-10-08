"""
Production Drift Monitor & Retrain Orchestrator
================================================
Monitors incoming production data batches against the training reference baseline.
Evaluates:
  1. Feature Data Drift (Covariate Shift)
  2. Data Quality & Summary Statistics (Missing values, distributions)
  3. Prediction Drift (Distribution shift in model inferences)
  4. Target Drift (Label shift, when ground-truth target is present)
  5. Concept Drift / Regression Quality (RMSE, MAE, R², when ground-truth target is present)

When drift is detected:
  - If ground-truth target labels persist in the new batch:
    Automatically triggers continuous training (CT) on combined (old + new) data.
  - If target labels are missing:
    Logs drift telemetry and alerts engineering (supervised retraining requires labels).
"""
import os
os.environ.setdefault("MPLBACKEND", "Agg")
import pandas as pd
from typing import Optional
from core.db.repository import BlackFridayRepository
from ml.models.registry import ModelRegistry
from ml.tracking.drift_monitor import DriftMonitor, DriftResult
from ml.tracking.mlflow_tracker import MLflowTracker
from ml.serving.predictor import ONNXPredictor
from ml.serving.imputer import ONNXMissForestImputer
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


def run_production_drift_monitor(
    current_batch_df: pd.DataFrame,
    reference_df: Optional[pd.DataFrame] = None,
    candidate_model_name: Optional[str] = None,
    dataset_drift_threshold: float = settings.DRIFT_DATASET_THRESHOLD,
    auto_trigger_retrain: bool = True
) -> DriftResult:
    """Compares incoming production data batch against the training reference baseline.

    Args:
        current_batch_df: Incoming production batch to monitor (Mandatory).
        reference_df: Historical reference baseline data. If None, queries the official
                      training split ('train') from the data warehouse.
        candidate_model_name: Model architecture to monitor and use for predictions.
        dataset_drift_threshold: Fraction of features that must drift to trigger alert/retrain.
        auto_trigger_retrain: If True and drift is detected, triggers CT retrain on combined data.

    Returns:
        DriftResult: Structured drift signals and audit paths (with retrain_result if triggered).
    """
    if current_batch_df is None or current_batch_df.empty:
        raise ValueError(
            "current_batch_df is required for production drift monitoring. "
            "Please provide an incoming batch of production data."
        )

    repo = BlackFridayRepository()
    tracker = MLflowTracker()
    model_name = ModelRegistry.resolve_name(candidate_model_name or settings.DEFAULT_REGRESSION_MODEL)

    logger.info("PRODUCTION DRIFT MONITOR: Starting batch vs reference comparison...")

    # 1. Dataset Resolution: Official Training Split as Reference Baseline
    if reference_df is None:
        logger.info("Loading official training baseline ('split=train') from warehouse...")
        reference_df = repo.get_cleaned_records_df(split="train", exclude_outliers=True)
        if reference_df.empty:
            raise ValueError("Training baseline partition not found in warehouse. Run preprocessing first.")

    # 2. Derive Features and Standardize Incoming Batch Schema
    candidate = ModelRegistry.get_model(model_name)
    features = candidate.features

    ref_eval = reference_df.copy()
    ref_eval.columns = [c.lower() for c in ref_eval.columns]

    curr_eval = current_batch_df.copy()
    curr_eval.columns = [c.lower() for c in curr_eval.columns]

    if "normalized_purchase" not in curr_eval.columns and "purchase" in curr_eval.columns:
        curr_eval["normalized_purchase"] = curr_eval["purchase"] / settings.PURCHASE_MAX

    # Impute missing categories if present using production ONNX imputer
    imputer_dir = os.path.join(settings.BASE_DIR, "models", "onnx", "imputer")
    if os.path.exists(os.path.join(imputer_dir, "imputer_metadata.json")):
        for cat_col in ["product_category_2", "product_category_3"]:
            if cat_col in curr_eval.columns and curr_eval[cat_col].isna().any():
                logger.info("Imputing missing categories using production ONNXMissForestImputer...")
                imputer = ONNXMissForestImputer(imputer_dir)
                curr_eval = imputer.transform(curr_eval)
                break

    # 3. Score Predictions using Production ONNX Model (Fast Inference, No Re-Fitting)
    onnx_path = os.path.join(settings.BASE_DIR, "models", "onnx", f"{model_name}.onnx")
    if not os.path.exists(onnx_path):
        onnx_path = os.path.join(settings.BASE_DIR, "models", "onnx", "champion.onnx")

    if not os.path.exists(onnx_path):
        logger.info(f"Local ONNX model '{model_name}.onnx' not found. Attempting MLflow Champion download...")
        try:
            tracker.download_champion_onnx(model_name=settings.MLFLOW_MODEL_NAME)
            onnx_path = os.path.join(settings.BASE_DIR, "models", "onnx", "champion.onnx")
        except Exception as e:
            logger.warning(f"Could not fetch Champion ONNX model from MLflow: {e}")

    if os.path.exists(onnx_path):
        logger.info(f"Scoring predictions with production ONNX Runtime engine: {os.path.basename(onnx_path)}")
        predictor = ONNXPredictor(onnx_path)
        ref_eval["prediction"] = predictor.predict(ref_eval[features])
        curr_eval["prediction"] = predictor.predict(curr_eval[features])
    else:
        logger.warning(f"No ONNX model available for '{model_name}'. Falling back to fitted pipeline...")
        y_ref = ref_eval["normalized_purchase"] if "normalized_purchase" in ref_eval.columns else ref_eval["purchase"]
        candidate.fit(ref_eval, y_ref)
        ref_eval["prediction"] = candidate.predict(ref_eval)
        curr_eval["prediction"] = candidate.predict(curr_eval)

    # Columns to monitor: Features + Prediction + Target (if present)
    monitored_cols = sorted(set(features)) + ["prediction"]
    for target in ["normalized_purchase", "purchase"]:
        if target in ref_eval.columns and target in curr_eval.columns:
            monitored_cols.append(target)
            break

    active_cols = [c for c in monitored_cols if c in ref_eval.columns and c in curr_eval.columns]
    logger.info(
        f"Reference Baseline (Old Data): {len(reference_df):,} rows | "
        f"Incoming Batch (New Data): {len(current_batch_df):,} rows | "
        f"Monitored Columns: {active_cols}"
    )

    # 3. Evaluate Drift with Evidently AI
    drift_monitor = DriftMonitor(reference_df=ref_eval[active_cols])
    drift_result = drift_monitor.evaluate_drift(
        current_df=curr_eval[active_cols],
        output_dir="reports/drift"
    )

    logger.info(
        f"Drift Analysis Complete -> Share: {drift_result.drifted_feature_share:.1%} "
        f"({drift_result.drifted_features}/{drift_result.total_features} features drifted) | "
        f"Target Drift: {drift_result.target_drift_detected} | "
        f"Prediction Drift: {drift_result.prediction_drift_detected} | "
        f"Concept Drift: {drift_result.concept_drift_detected}"
    )

    # 4. Log Drift Telemetry to MLflow
    drift_metrics = {
        "drift_drifted_feature_share": drift_result.drifted_feature_share,
        "drift_drifted_features": float(drift_result.drifted_features),
        "drift_total_features": float(drift_result.total_features),
        "reference_rows": float(len(reference_df)),
        "current_batch_rows": float(len(current_batch_df)),
    }
    if drift_result.regression_performance:
        for k, v in drift_result.regression_performance.items():
            if v is not None:
                drift_metrics[f"concept_{k}"] = float(v)

    drift_tags = {
        "pipeline_stage": "production_drift_monitor",
        "dataset_drift_detected": str(drift_result.dataset_drift_detected),
        "target_drift_detected": str(drift_result.target_drift_detected),
        "prediction_drift_detected": str(drift_result.prediction_drift_detected),
        "concept_drift_detected": str(drift_result.concept_drift_detected),
        "drift_engine": drift_result.engine,
        "retrain_threshold": str(dataset_drift_threshold),
        "model_name": model_name,
        "drifted_columns": ",".join(drift_result.drifted_columns),
    }

    drift_artifacts = {}
    if drift_result.report_path and os.path.exists(drift_result.report_path):
        drift_artifacts["data_drift_report"] = drift_result.report_path
    if drift_result.json_path and os.path.exists(drift_result.json_path):
        drift_artifacts["drift_summary_json"] = drift_result.json_path
        full_json = drift_result.json_path.replace(".json", "_full_evidently.json")
        if os.path.exists(full_json):
            drift_artifacts["full_evidently_metrics_json"] = full_json

    tracker.log_drift_run(
        model_name=model_name,
        params={
            "reference_rows": len(reference_df),
            "current_batch_rows": len(current_batch_df),
            "dataset_drift_threshold": dataset_drift_threshold,
            "monitored_columns_count": len(active_cols)
        },
        metrics=drift_metrics,
        tags=drift_tags,
        artifacts=drift_artifacts
    )

    # 5. Drift Decision Gate with Target Persistence Check
    should_retrain = drift_result.should_trigger_retrain(dataset_threshold=dataset_drift_threshold)
    target_col = "normalized_purchase"
    has_target = (
        (target_col in curr_eval.columns and curr_eval[target_col].notna().any())
        or ("purchase" in current_batch_df.columns and current_batch_df["purchase"].notna().any())
    )

    if should_retrain:
        logger.warning(
            f"🚨 DRIFT TRIGGER DETECTED! "
            f"drifted_share={drift_result.drifted_feature_share:.1%} >= threshold={dataset_drift_threshold:.1%} "
            f"(target_drift={drift_result.target_drift_detected}, "
            f"pred_drift={drift_result.prediction_drift_detected}, "
            f"concept_drift={drift_result.concept_drift_detected})."
        )

        if not has_target:
            logger.warning(
                f"⚠️ DRIFT DETECTED BUT GROUND-TRUTH TARGET IS MISSING: "
                f"Incoming batch does not contain '{target_col}'. "
                f"Supervised retraining cannot proceed without ground-truth labels. "
                f"Logged drift alerts to MLflow telemetry; awaiting labeled ground-truth data."
            )
        elif auto_trigger_retrain:
            logger.info(
                f"Ground-truth target '{target_col}' is present in new batch. "
                f"Triggering automated retrain pipeline on combined dataset: "
                f"OLD data ({len(reference_df):,} rows) + NEW data ({len(current_batch_df):,} rows)..."
            )
            from ml.pipelines.retrain import run_champion_challenger_retrain

            retrain_new_data = curr_eval if target_col in curr_eval.columns else current_batch_df
            retrain_result = run_champion_challenger_retrain(
                old_data_df=reference_df,
                new_data_df=retrain_new_data,
                drift_result=drift_result,
                candidate_model_name=model_name,
                trigger_reason="drift_detected",
                min_improvement_delta=settings.CHAMPION_MIN_IMPROVEMENT_DELTA,
                min_r2_threshold=settings.CHAMPION_MIN_R2_THRESHOLD,
            )

            drift_result.retrain_result = retrain_result

            if retrain_result.is_promoted:
                logger.info(
                    f"🏆 CONTINUOUS TRAINING SUCCESS: Candidate '{model_name}' defeated the champion "
                    f"(R2: {retrain_result.candidate_r2:.4f} vs {retrain_result.champion_r2:.4f}, "
                    f"delta: {retrain_result.improvement_delta:+.4f}) and is PROMOTED to Production!"
                )
            else:
                logger.info(
                    f"⚠️ CONTINUOUS TRAINING COMPLETE: Candidate '{model_name}' did not defeat champion "
                    f"({retrain_result.rejection_reason}). Incumbent Production Champion retained."
                )
        else:
            logger.warning("auto_trigger_retrain=False — Continuous Training was not executed automatically.")
    else:
        logger.info(
            f"✅ NO SIGNIFICANT DRIFT DETECTED — "
            f"drifted_share={drift_result.drifted_feature_share:.1%} < threshold={dataset_drift_threshold:.1%}. "
            f"Incumbent Champion remains stable. No retraining required."
        )

    return drift_result


if __name__ == "__main__":
    import argparse
    import sys

    parser = argparse.ArgumentParser(
        description="Black Friday Production Drift Monitor & Automated Retrain Trigger",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument(
        "--batch", "-b",
        type=str,
        default=None,
        help="Path to CSV containing incoming production batch data"
    )
    parser.add_argument(
        "--reference", "-r",
        type=str,
        default=None,
        help="Path to CSV containing baseline reference data (defaults to warehouse train split)"
    )
    parser.add_argument(
        "--model", "-m",
        type=str,
        default=None,
        help="Candidate model architecture (e.g. lgbm, random_forest). Defaults to DEFAULT_REGRESSION_MODEL."
    )
    parser.add_argument(
        "--threshold", "-t",
        type=float,
        default=settings.DRIFT_DATASET_THRESHOLD,
        help="Dataset drift threshold to trigger continuous retraining"
    )
    parser.add_argument(
        "--no-retrain",
        action="store_true",
        help="Disable automatic retraining trigger if drift is detected"
    )
    parser.add_argument(
        "--demo",
        action="store_true",
        help="Run monitoring in demonstration mode using warehouse holdout test batch"
    )

    args = parser.parse_args()

    # Load reference dataset if provided
    ref_df = None
    if args.reference:
        if not os.path.exists(args.reference):
            logger.error(f"Reference file not found: {args.reference}")
            sys.exit(1)
        ref_df = pd.read_csv(args.reference)
        logger.info(f"Loaded reference dataset from {args.reference} ({len(ref_df):,} rows)")

    # Load incoming batch dataset
    if args.batch:
        if not os.path.exists(args.batch):
            logger.error(f"Incoming batch file not found: {args.batch}")
            sys.exit(1)
        curr_batch_df = pd.read_csv(args.batch)
        logger.info(f"Loaded incoming batch from {args.batch} ({len(curr_batch_df):,} rows)")
    else:
        logger.info(
            "💡 No --batch provided. Running demonstration mode with warehouse holdout test batch... "
            "(To monitor a custom batch, run: make monitor BATCH=path/to/batch.csv)"
        )
        repo = BlackFridayRepository()
        curr_batch_df = repo.get_cleaned_records_df(split="test", exclude_outliers=True)
        if curr_batch_df.empty:
            curr_batch_df = repo.get_cleaned_records_df(exclude_outliers=True).tail(3000)
        logger.info(f"Extracted demo test batch ({len(curr_batch_df):,} rows).")

    run_production_drift_monitor(
        current_batch_df=curr_batch_df,
        reference_df=ref_df,
        candidate_model_name=args.model,
        dataset_drift_threshold=args.threshold,
        auto_trigger_retrain=not args.no_retrain
    )
