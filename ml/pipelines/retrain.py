import os
import json
import datetime
import pandas as pd
from typing import Optional, Dict, Any
from core.db.repository import BlackFridayRepository
from ml.features.preprocessor import DataPreprocessor
from ml.models.registry import ModelRegistry
from ml.models.metrics import ModelEvaluator
from ml.models.onnx_exporter import ONNXExporter
from ml.tracking.mlflow_tracker import MLflowTracker
from ml.tracking.model_card import ModelCardGenerator
from ml.tracking.drift_monitor import DriftResult
from ml.tracking.explainability import ModelExplainability
from ml.visualization.visualizer import Visualizer
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class RetrainResult:
    """Structured result of Champion vs Challenger continuous training."""

    def __init__(
        self,
        is_promoted: bool,
        candidate_model_name: str,
        candidate_r2: float,
        candidate_rmse: float,
        champion_r2: float,
        champion_rmse: float,
        improvement_delta: float,
        run_id: Optional[str],
        drift_result: Optional[DriftResult],
        data_summary: Dict[str, Any],
        audit_summary_path: Optional[str],
        metrics: Dict[str, Any],
        rejection_reason: Optional[str] = None
    ):
        self.is_promoted = is_promoted
        self.candidate_model_name = candidate_model_name
        self.candidate_r2 = round(float(candidate_r2), 4)
        self.candidate_rmse = round(float(candidate_rmse), 4)
        self.champion_r2 = round(float(champion_r2), 4)
        self.champion_rmse = round(float(champion_rmse), 4)
        self.improvement_delta = round(float(improvement_delta), 4)
        self.run_id = run_id
        self.drift_result = drift_result
        self.data_summary = data_summary
        self.audit_summary_path = audit_summary_path
        self.metrics = metrics
        self.rejection_reason = rejection_reason

    def __bool__(self) -> bool:
        """Enables pythonic `if result:` syntax for promotion checking."""
        return self.is_promoted

    def to_dict(self) -> Dict[str, Any]:
        return {
            "is_promoted": self.is_promoted,
            "candidate_model_name": self.candidate_model_name,
            "candidate_r2": self.candidate_r2,
            "candidate_rmse": self.candidate_rmse,
            "champion_r2": self.champion_r2,
            "champion_rmse": self.champion_rmse,
            "improvement_delta": self.improvement_delta,
            "run_id": self.run_id,
            "rejection_reason": self.rejection_reason,
            "drift_info": self.drift_result.to_dict() if self.drift_result else None,
            "data_summary": self.data_summary,
            "audit_summary_path": self.audit_summary_path,
            "metrics": self.metrics
        }

    def __repr__(self) -> str:
        status = "PROMOTED" if self.is_promoted else "REJECTED"
        return (
            f"RetrainResult({status} | candidate='{self.candidate_model_name}' "
            f"R2={self.candidate_r2:.4f} vs champion={self.champion_r2:.4f} "
            f"(delta={self.improvement_delta:+.4f}) | run_id={self.run_id})"
        )


def run_champion_challenger_retrain(
    old_data_df: pd.DataFrame,
    new_data_df: pd.DataFrame,
    drift_result: Optional[DriftResult] = None,
    candidate_model_name: Optional[str] = None,
    trigger_reason: str = "drift_detected",
    min_improvement_delta: float = settings.CHAMPION_MIN_IMPROVEMENT_DELTA,
    min_r2_threshold: float = settings.CHAMPION_MIN_R2_THRESHOLD,
    cv_splits: int = 3,
) -> RetrainResult:
    """Automated Champion vs Challenger Continuous Training (CT) workflow.

    Triggered exclusively after drift detection on incoming production batches.
    Retrains candidate architecture on combined (old reference + new drifted) data,
    evaluating against MLflow Model Registry Champion (SSOT) and logging full
    drift telemetry and training diagnostics.

    Args:
        old_data_df: Historical reference baseline data (old data).
        new_data_df: Latest incoming production data batch with ground-truth target (new data).
        drift_result: The DriftResult instance that triggered this continuous retraining.
        candidate_model_name: Architecture to evaluate (defaults to DEFAULT_REGRESSION_MODEL).
        trigger_reason: Reason for execution (e.g. 'drift_detected').
        min_improvement_delta: Minimum R2 margin candidate must beat champion by.
        min_r2_threshold: Minimum absolute R2 score required for production promotion.
        cv_splits: Number of cross-validation folds.

    Returns:
        RetrainResult: Structured result evaluating to True if candidate was promoted.
    """
    if old_data_df is None or old_data_df.empty:
        raise ValueError("old_data_df is required for continuous retraining on combined data.")
    if new_data_df is None or new_data_df.empty:
        raise ValueError("new_data_df is required for continuous retraining on combined data.")

    target_col = "normalized_purchase"
    if target_col not in new_data_df.columns:
        raise ValueError(
            f"new_data_df must contain ground-truth '{target_col}' column for supervised retraining."
        )

    model_name = candidate_model_name or settings.DEFAULT_REGRESSION_MODEL
    logger.info("=" * 70)
    logger.info(f"CONTINUOUS TRAINING: Evaluating candidate '{model_name}' (trigger='{trigger_reason}')...")
    logger.info("=" * 70)

    preprocessor = DataPreprocessor()
    evaluator = ModelEvaluator(n_splits=cv_splits)
    tracker = MLflowTracker()

    # 1. Dataset Resolution: Combine Old Baseline + New Production Batch
    old_rows = len(old_data_df)
    new_rows = len(new_data_df)
    df = pd.concat([old_data_df, new_data_df], ignore_index=True)
    data_source = "combined_old_plus_new_batches"
    logger.info(
        f"Training on combined dataset: {old_rows:,} old records + {new_rows:,} new records "
        f"= {len(df):,} total training records."
    )

    # 2. Log Drift Context if provided
    if drift_result:
        logger.info(
            f"Retrain linked to drift event: drifted_share={drift_result.drifted_feature_share:.1%}, "
            f"target_drift={drift_result.target_drift_detected}, "
            f"prediction_drift={drift_result.prediction_drift_detected}, "
            f"concept_drift={drift_result.concept_drift_detected}, engine={drift_result.engine}"
        )
    else:
        logger.info(f"Retrain execution initiated standalone (trigger_reason='{trigger_reason}').")

    # 3. Train/Holdout Test Split (90/10)
    train_df, test_df = preprocessor.split_train_test(df, test_size=0.10)
    y_train = train_df[target_col]
    y_test = test_df[target_col]

    # 4. Train Candidate Model & Cross-Validate
    candidate = ModelRegistry.get_model(model_name)
    logger.info(f"Running {cv_splits}-fold Cross-Validation for candidate '{model_name}' on {len(train_df):,} records...")
    cv_metrics = evaluator.cross_validate(candidate, train_df, y_train)

    logger.info(f"Fitting candidate '{model_name}' on full combined training partition ({len(train_df):,} records)...")
    candidate.fit(train_df, y_train)

    test_preds = candidate.predict(test_df)
    holdout_metrics = evaluator.evaluate_holdout(y_test.to_numpy(), test_preds)
    candidate_r2 = holdout_metrics["r2"]
    candidate_rmse = holdout_metrics["rmse"]

    logger.info(f"Candidate '{model_name}' Holdout Test -> R2: {candidate_r2:.4f}, RMSE: {candidate_rmse:.4f}")

    # 5. Query Active Champion from MLflow Registry (SSOT)
    champion_info = tracker.get_champion_metrics(model_name=settings.MLFLOW_MODEL_NAME)
    champion_r2 = float(champion_info.get("r2", 0.0))
    champion_rmse = float(champion_info.get("rmse", float("inf")))
    improvement_delta = candidate_r2 - champion_r2

    logger.info(
        f"Active MLflow Champion -> R2: {champion_r2:.4f}, RMSE: {champion_rmse:.4f} | "
        f"Candidate Delta: {improvement_delta:+.4f} (Required Min Delta: {min_improvement_delta:+.4f}, "
        f"Min Absolute R2: {min_r2_threshold:.4f})"
    )

    # 6. Champion vs Challenger Decision Gate
    meets_r2_threshold = candidate_r2 >= min_r2_threshold
    beats_champion_margin = candidate_r2 >= (champion_r2 + min_improvement_delta)
    is_promoted = meets_r2_threshold and beats_champion_margin

    rejection_reason = None
    if not is_promoted:
        if not meets_r2_threshold:
            rejection_reason = f"Candidate R2 ({candidate_r2:.4f}) below minimum absolute threshold ({min_r2_threshold:.4f})"
        else:
            rejection_reason = f"Candidate R2 ({candidate_r2:.4f}) failed to beat champion ({champion_r2:.4f}) by required delta ({min_improvement_delta:.4f})"

    # 7. Assemble Audit Record & Local Artifacts
    os.makedirs("reports/retrain", exist_ok=True)
    timestamp_str = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    audit_path = os.path.join("reports", "retrain", f"retrain_audit_{model_name}_{timestamp_str}.json")
    latest_audit_path = os.path.join("reports", "retrain", "latest_retrain_audit.json")

    data_summary = {
        "data_source": data_source,
        "old_records_count": old_rows,
        "new_records_count": new_rows,
        "combined_total_records": len(df),
        "train_records": len(train_df),
        "test_records": len(test_df),
    }

    audit_payload = {
        "timestamp": datetime.datetime.now().isoformat(),
        "candidate_model_name": model_name,
        "trigger_reason": trigger_reason,
        "is_promoted": is_promoted,
        "rejection_reason": rejection_reason,
        "data_summary": data_summary,
        "champion_comparison": {
            "active_champion_r2": champion_r2,
            "active_champion_rmse": champion_rmse,
            "candidate_r2": candidate_r2,
            "candidate_rmse": candidate_rmse,
            "r2_improvement_delta": round(improvement_delta, 4),
            "min_r2_threshold": min_r2_threshold,
            "min_improvement_delta": min_improvement_delta
        },
        "drift_summary": drift_result.to_dict() if drift_result else None,
        "cv_metrics": cv_metrics,
        "holdout_metrics": holdout_metrics
    }

    for p in [audit_path, latest_audit_path]:
        with open(p, "w", encoding="utf-8") as f:
            json.dump(audit_payload, f, indent=2)

    # 8. Assemble MLflow Run Logging Data
    all_metrics = {**cv_metrics, **{f"test_{k}": v for k, v in holdout_metrics.items()}}
    mlflow_metrics = {
        **all_metrics,
        "champion_r2": champion_r2,
        "candidate_r2": candidate_r2,
        "r2_improvement_delta": round(improvement_delta, 4),
        "data_combined_rows": float(len(df)),
        "data_train_rows": float(len(train_df)),
        "data_test_rows": float(len(test_df)),
        "data_old_rows": float(old_rows),
        "data_new_rows": float(new_rows),
    }
    if drift_result:
        mlflow_metrics.update({
            "drift_drifted_feature_share": drift_result.drifted_feature_share,
            "drift_drifted_features": float(drift_result.drifted_features),
            "drift_total_features": float(drift_result.total_features),
            "drift_dataset_drift_detected": 1.0 if drift_result.dataset_drift_detected else 0.0,
            "drift_target_drift_detected": 1.0 if drift_result.target_drift_detected else 0.0,
            "drift_prediction_drift_detected": 1.0 if drift_result.prediction_drift_detected else 0.0,
        })

    mlflow_tags = {
        "trigger_reason": trigger_reason,
        "model_name": model_name,
        "model_architecture": model_name,
        "retrain_dataset_source": data_source,
        "deployment_status": "PROMOTED_TO_PRODUCTION" if is_promoted else "REJECTED",
        "pipeline_stage": "retrain_champion_challenger",
    }
    if drift_result:
        mlflow_tags.update({
            "dataset_drift_detected": str(drift_result.dataset_drift_detected),
            "target_drift_detected": str(drift_result.target_drift_detected),
            "prediction_drift_detected": str(drift_result.prediction_drift_detected),
            "drift_engine": drift_result.engine,
            "drifted_columns": ",".join(drift_result.drifted_columns),
        })

    artifacts: Dict[str, str] = {
        "retrain_audit_summary": audit_path
    }
    if drift_result:
        if drift_result.report_path and os.path.exists(drift_result.report_path):
            artifacts["data_drift_report"] = drift_result.report_path
        if drift_result.json_path and os.path.exists(drift_result.json_path):
            artifacts["drift_summary_json"] = drift_result.json_path

    # SHAP Explainability
    logger.info(f"Computing SHAP feature attributions for candidate '{model_name}'...")
    bg_sample = train_df[candidate.features].head(100)
    explainer = ModelExplainability(candidate.pipeline, background_sample=bg_sample)
    shap_values, X_eval = explainer.explain(test_df[candidate.features].head(200))
    if shap_values is not None:
        shap_plot_path = Visualizer.generate_shap_summary_plot(
            shap_values=shap_values,
            eval_features=X_eval,
            output_dir=f"reports/shap_{model_name}"
        )
        if shap_plot_path:
            artifacts["shap_summary_plot"] = shap_plot_path

    # Model Card & Fairness
    fairness = ModelEvaluator.evaluate_fairness(candidate, test_df)
    card_path = ModelCardGenerator.generate_model_card(model_name, all_metrics, fairness)
    if card_path:
        artifacts["model_card"] = card_path

    # 9. Promotion or Rejection Execution
    run_id = None
    onnx_file = None
    if is_promoted:
        logger.info(f"🏆 PROMOTION GRANTED: Candidate '{model_name}' is NEW CHAMPION! Exporting ONNX...")
        onnx_dir = os.path.join(settings.BASE_DIR, "models", "onnx")
        os.makedirs(onnx_dir, exist_ok=True)
        onnx_file = os.path.join(onnx_dir, f"{model_name}.onnx")
        ONNXExporter.export_regression_pipeline(candidate.pipeline, candidate.features, onnx_file)
        ONNXExporter.verify_parity(candidate, onnx_file, test_df[candidate.features].head(50))
        mlflow_tags["promoted_by"] = "automated_ct_pipeline"
    else:
        logger.warning(
            f"⚠️ PROMOTION REJECTED: Candidate '{model_name}' failed promotion gate. "
            f"Reason: {rejection_reason}. Incumbent Champion retained."
        )
        mlflow_tags["rejection_reason"] = rejection_reason or "Unknown"

    run_id = tracker.log_run(
        run_name=f"{'champion' if is_promoted else 'challenger_rejected'}_{model_name}_{trigger_reason}",
        params=candidate.get_params(),
        metrics=mlflow_metrics,
        tags=mlflow_tags,
        artifacts=artifacts,
        onnx_model_path=onnx_file if is_promoted else None
    )

    if is_promoted and run_id:
        tracker.promote_to_champion(run_id=run_id, model_name=settings.MLFLOW_MODEL_NAME)
        tracker.download_champion_onnx(model_name=settings.MLFLOW_MODEL_NAME)
        logger.info(f"✅ Champion promotion complete in MLflow for '{model_name}' (Run ID: {run_id}).")

    retrain_result = RetrainResult(
        is_promoted=is_promoted,
        candidate_model_name=model_name,
        candidate_r2=candidate_r2,
        candidate_rmse=candidate_rmse,
        champion_r2=champion_r2,
        champion_rmse=champion_rmse,
        improvement_delta=improvement_delta,
        run_id=run_id,
        drift_result=drift_result,
        data_summary=data_summary,
        audit_summary_path=audit_path,
        metrics=all_metrics,
        rejection_reason=rejection_reason
    )

    if drift_result:
        drift_result.retrain_result = retrain_result
    return retrain_result


if __name__ == "__main__":
    import argparse
    import sys

    parser = argparse.ArgumentParser(
        description="Black Friday Champion vs Challenger Retrain Pipeline",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument(
        "--new-data", "-n",
        type=str,
        default=None,
        help="Path to CSV containing new data batch"
    )
    parser.add_argument(
        "--old-data", "-o",
        type=str,
        default=None,
        help="Path to CSV containing old baseline data (defaults to warehouse train split)"
    )
    parser.add_argument(
        "--model", "-m",
        type=str,
        default=settings.DEFAULT_REGRESSION_MODEL,
        help="Candidate challenger model architecture"
    )
    parser.add_argument(
        "--min-improvement",
        type=float,
        default=settings.CHAMPION_MIN_IMPROVEMENT_DELTA,
        help="Minimum R2 improvement required over Champion"
    )
    parser.add_argument(
        "--min-r2",
        type=float,
        default=settings.CHAMPION_MIN_R2_THRESHOLD,
        help="Minimum absolute R2 threshold required"
    )
    parser.add_argument(
        "--demo",
        action="store_true",
        help="Run retraining demonstration using warehouse partitions"
    )

    args = parser.parse_args()
    repo = BlackFridayRepository()

    # Old data resolution
    if args.old_data:
        if not os.path.exists(args.old_data):
            logger.error(f"Old data file not found: {args.old_data}")
            sys.exit(1)
        old_df = pd.read_csv(args.old_data)
        logger.info(f"Loaded old baseline data from {args.old_data} ({len(old_df):,} rows)")
    else:
        logger.info("Loading baseline reference from warehouse (split='train')...")
        old_df = repo.get_cleaned_records_df(split="train", exclude_outliers=True)

    # New data resolution
    if args.new_data:
        if not os.path.exists(args.new_data):
            logger.error(f"New data file not found: {args.new_data}")
            sys.exit(1)
        new_df = pd.read_csv(args.new_data)
        logger.info(f"Loaded new incoming data from {args.new_data} ({len(new_df):,} rows)")
    else:
        logger.info(
            "💡 No --new-data provided. Running demonstration mode using warehouse holdout test partition... "
            "(To retrain on a custom batch, run: make retrain NEW_DATA=path/to/new.csv)"
        )
        new_df = repo.get_cleaned_records_df(split="test", exclude_outliers=True)
        if new_df.empty:
            new_df = repo.get_cleaned_records_df(exclude_outliers=True).tail(3000)

    # Standardize schema and derive normalized_purchase if raw purchase column is passed
    def _prepare_retrain_batch(df: pd.DataFrame) -> pd.DataFrame:
        df = df.copy()
        df.columns = [c.lower() for c in df.columns]
        if "normalized_purchase" not in df.columns and "purchase" in df.columns:
            df["normalized_purchase"] = df["purchase"] / settings.PURCHASE_MAX

        imputer_dir = os.path.join(settings.BASE_DIR, "models", "onnx", "imputer")
        if os.path.exists(os.path.join(imputer_dir, "imputer_metadata.json")):
            for cat_col in ["product_category_2", "product_category_3"]:
                if cat_col in df.columns and df[cat_col].isna().any():
                    logger.info("Imputing missing categories in retrain batch using ONNXMissForestImputer...")
                    from ml.serving.imputer import ONNXMissForestImputer
                    imputer = ONNXMissForestImputer(imputer_dir)
                    df = imputer.transform(df)
                    break
        return df

    old_df = _prepare_retrain_batch(old_df)
    new_df = _prepare_retrain_batch(new_df)

    # Verify target persistence in new_df
    target_col = "normalized_purchase"
    if target_col not in new_df.columns:
        logger.error(
            f"Cannot proceed with supervised continuous retraining: "
            f"target column '{target_col}' not found in new data batch."
        )
        sys.exit(1)

    result = run_champion_challenger_retrain(
        old_data_df=old_df,
        new_data_df=new_df,
        candidate_model_name=args.model,
        trigger_reason="manual_cli_execution",
        min_improvement_delta=args.min_improvement,
        min_r2_threshold=args.min_r2
    )

    logger.info(
        f"Retrain completed: is_promoted={result.is_promoted}, "
        f"candidate_r2={result.candidate_r2:.4f}, delta={result.improvement_delta:+.4f}"
    )
