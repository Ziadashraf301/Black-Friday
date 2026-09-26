import os
import json
import pandas as pd
import numpy as np
from typing import Optional, Dict, Any, List
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

from evidently.legacy.report import Report
from evidently.legacy.pipeline.column_mapping import ColumnMapping
from evidently.legacy.metric_preset import (
    DataDriftPreset,
    TargetDriftPreset,
    DataQualityPreset,
    RegressionPreset
)


class DriftResult:
    """Structured result returned by DriftMonitor — exposes programmatic drift signals and audit data."""

    def __init__(
        self,
        dataset_drift_detected: bool,
        drifted_feature_share: float,
        drifted_features: int,
        total_features: int,
        target_drift_detected: bool,
        prediction_drift_detected: bool,
        report_path: Optional[str],
        engine: str = "evidently",
        drifted_columns: Optional[List[str]] = None,
        column_drift_scores: Optional[Dict[str, Dict[str, Any]]] = None,
        concept_drift_detected: bool = False,
        regression_performance: Optional[Dict[str, Any]] = None,
        data_quality_summary: Optional[Dict[str, Any]] = None,
        json_path: Optional[str] = None,
        retrain_result: Optional[Any] = None
    ):
        self.dataset_drift_detected = dataset_drift_detected
        self.drifted_feature_share = round(float(drifted_feature_share), 4)
        self.drifted_features = int(drifted_features)
        self.total_features = int(total_features)
        self.target_drift_detected = bool(target_drift_detected)
        self.prediction_drift_detected = bool(prediction_drift_detected)
        self.concept_drift_detected = bool(concept_drift_detected)
        self.regression_performance = regression_performance or {}
        self.data_quality_summary = data_quality_summary or {}
        self.report_path = report_path
        self.json_path = json_path
        self.engine = engine
        self.drifted_columns = drifted_columns or []
        self.column_drift_scores = column_drift_scores or {}
        self.retrain_result = retrain_result

    def should_trigger_retrain(
        self,
        dataset_threshold: Optional[float] = None,
    ) -> bool:
        """Returns True if drift severity crosses the continuous retraining threshold.

        Retraining is triggered when:
          - More than `dataset_threshold` fraction of features have drifted (Covariate Shift), OR
          - Target distribution drift is detected (Label Shift), OR
          - Prediction distribution drift is detected, OR
          - Concept drift / regression error spike is detected.
        """
        threshold = dataset_threshold if dataset_threshold is not None else settings.DRIFT_DATASET_THRESHOLD
        return (
            (self.drifted_feature_share >= threshold)
            or self.target_drift_detected
            or self.prediction_drift_detected
            or self.concept_drift_detected
        )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "dataset_drift_detected": self.dataset_drift_detected,
            "drifted_feature_share": self.drifted_feature_share,
            "drifted_features": self.drifted_features,
            "total_features": self.total_features,
            "target_drift_detected": self.target_drift_detected,
            "prediction_drift_detected": self.prediction_drift_detected,
            "concept_drift_detected": self.concept_drift_detected,
            "regression_performance": self.regression_performance,
            "data_quality_summary": self.data_quality_summary,
            "report_path": self.report_path,
            "json_path": self.json_path,
            "engine": self.engine,
            "drifted_columns": self.drifted_columns,
            "column_drift_scores": self.column_drift_scores,
            "retrain_triggered": self.retrain_result is not None,
            "retrain_promoted": getattr(self.retrain_result, "is_promoted", False) if self.retrain_result else None
        }

    def __bool__(self) -> bool:
        return self.should_trigger_retrain()

    def __repr__(self) -> str:
        return (
            f"DriftResult(dataset_drift={self.dataset_drift_detected}, "
            f"share={self.drifted_feature_share:.1%}, "
            f"drifted_cols={self.drifted_columns}, "
            f"target_drift={self.target_drift_detected}, "
            f"prediction_drift={self.prediction_drift_detected}, "
            f"concept_drift={self.concept_drift_detected}, "
            f"engine='{self.engine}')"
        )


class DriftMonitor:
    """Computes Data Drift, Target Drift, Prediction Drift, Data Quality, and Concept Drift via built-in Evidently AI Presets."""

    def __init__(self, reference_df: pd.DataFrame):
        self.reference_df = reference_df

    def evaluate_drift(
        self,
        current_df: pd.DataFrame,
        output_dir: str = "reports/drift"
    ) -> DriftResult:
        """Generates comprehensive drift report comparing reference baseline vs incoming batch using Evidently AI."""
        os.makedirs(output_dir, exist_ok=True)
        html_path = os.path.join(output_dir, "data_drift_report.html")
        json_path = os.path.join(output_dir, "drift_summary.json")

        logger.info("Configuring Evidently AI ColumnMapping & Built-in Presets...")

        # 1. Identify Target and Prediction columns
        target_col = None
        for candidate in ["normalized_purchase", "purchase"]:
            if candidate in self.reference_df.columns and candidate in current_df.columns:
                target_col = candidate
                break

        pred_col = None
        for candidate in ["prediction", "y_pred", "predicted_purchase"]:
            if candidate in self.reference_df.columns and candidate in current_df.columns:
                pred_col = candidate
                break

        # 2. Automatically classify Numerical vs Categorical features
        cols_to_check = [c for c in self.reference_df.columns if c in current_df.columns and c not in [target_col, pred_col]]
        num_features: List[str] = []
        cat_features: List[str] = []
        for c in cols_to_check:
            if pd.api.types.is_numeric_dtype(self.reference_df[c]) and self.reference_df[c].nunique() > 10:
                num_features.append(c)
            else:
                cat_features.append(c)

        column_mapping = ColumnMapping(
            target=target_col,
            prediction=pred_col,
            numerical_features=num_features,
            categorical_features=cat_features
        )

        # 3. Assemble built-in Evidently Metric Presets
        presets = [DataDriftPreset(), DataQualityPreset()]
        if target_col is not None:
            presets.append(TargetDriftPreset())
        if target_col is not None and pred_col is not None:
            presets.append(RegressionPreset())

        preset_names = [p.__class__.__name__ for p in presets]
        logger.info(f"Running Evidently Report with {len(presets)} presets: {preset_names} (target='{target_col}', pred='{pred_col}')...")

        report = Report(metrics=presets)
        report.run(
            reference_data=self.reference_df,
            current_data=current_df,
            column_mapping=column_mapping
        )
        report.save_html(html_path)

        report_dict = report.as_dict()
        metrics_list = report_dict.get("metrics", [])

        dataset_drift_detected = False
        drifted_features = 0
        total_features = len(cols_to_check) or len(self.reference_df.columns)
        target_drift_detected = False
        prediction_drift_detected = False
        concept_drift_detected = False
        drifted_columns: List[str] = []
        column_drift_scores: Dict[str, Dict[str, Any]] = {}
        regression_performance: Dict[str, Any] = {}
        data_quality_summary: Dict[str, Any] = {}

        # 4. Extract structured statistical signals from Evidently metrics
        for metric in metrics_list:
            metric_id = metric.get("metric", "")
            result_data = metric.get("result", {})

            # Data Drift (Covariate Shift on Features)
            if "DatasetDriftMetric" in metric_id:
                dataset_drift_detected = bool(result_data.get("dataset_drift", False))
                drifted_features = int(result_data.get("number_of_drifted_columns", 0))
                total_features = int(result_data.get("number_of_columns", total_features))
                drift_by_columns = result_data.get("drift_by_columns", {})
                for col_name, col_info in drift_by_columns.items():
                    is_drifted = bool(col_info.get("drift_detected", False))
                    p_val = float(col_info.get("drift_score", 1.0))
                    stat_name = col_info.get("stattest_name", "evidently_default")
                    threshold = float(col_info.get("stattest_threshold", 0.05))

                    column_drift_scores[col_name] = {
                        "drift_detected": is_drifted,
                        "drift_score": round(p_val, 4),
                        "stat_test": stat_name,
                        "threshold": threshold,
                        "current_mean": round(float(col_info.get("current", {}).get("mean", 0)), 4) if isinstance(col_info.get("current"), dict) and "mean" in col_info.get("current") else None,
                        "reference_mean": round(float(col_info.get("reference", {}).get("mean", 0)), 4) if isinstance(col_info.get("reference"), dict) and "mean" in col_info.get("reference") else None,
                    }
                    if is_drifted:
                        drifted_columns.append(col_name)
                        if col_name == pred_col:
                            prediction_drift_detected = True

            # Target Drift (Label Shift)
            elif "ColumnDriftMetric" in metric_id or "TargetDrift" in metric_id:
                if result_data.get("drift_detected", False):
                    target_drift_detected = True

            # Concept Drift / Regression Quality (Error Metric Shifts)
            elif "RegressionQualityMetric" in metric_id or "Regression" in metric_id:
                curr_metrics = result_data.get("current", {})
                ref_metrics = result_data.get("reference", {})
                curr_rmse = curr_metrics.get("rmse")
                ref_rmse = ref_metrics.get("rmse")
                curr_r2 = curr_metrics.get("r2_score")
                ref_r2 = ref_metrics.get("r2_score")
                curr_mae = curr_metrics.get("mean_abs_error")
                ref_mae = ref_metrics.get("mean_abs_error")

                regression_performance = {
                    "current_rmse": round(float(curr_rmse), 4) if curr_rmse is not None else None,
                    "reference_rmse": round(float(ref_rmse), 4) if ref_rmse is not None else None,
                    "current_r2": round(float(curr_r2), 4) if curr_r2 is not None else None,
                    "reference_r2": round(float(ref_r2), 4) if ref_r2 is not None else None,
                    "current_mae": round(float(curr_mae), 4) if curr_mae is not None else None,
                    "reference_mae": round(float(ref_mae), 4) if ref_mae is not None else None,
                }

                # Flag concept drift if error increases by >20%
                if curr_rmse is not None and ref_rmse is not None and ref_rmse > 0:
                    if (curr_rmse - ref_rmse) / ref_rmse > 0.20:
                        concept_drift_detected = True

            # Data Quality Summary (Statistics, Missing Values)
            elif "DatasetSummaryMetric" in metric_id or "DataQuality" in metric_id:
                curr_summary = result_data.get("current", {})
                ref_summary = result_data.get("reference", {})
                data_quality_summary = {
                    "current_rows": curr_summary.get("number_of_rows"),
                    "reference_rows": ref_summary.get("number_of_rows"),
                    "current_missing_values": curr_summary.get("number_of_missing_values"),
                    "reference_missing_values": ref_summary.get("number_of_missing_values"),
                }

        drifted_share = drifted_features / max(total_features, 1)

        drift_result = DriftResult(
            dataset_drift_detected=dataset_drift_detected,
            drifted_feature_share=drifted_share,
            drifted_features=drifted_features,
            total_features=total_features,
            target_drift_detected=target_drift_detected,
            prediction_drift_detected=prediction_drift_detected,
            concept_drift_detected=concept_drift_detected,
            regression_performance=regression_performance,
            data_quality_summary=data_quality_summary,
            report_path=html_path,
            json_path=json_path,
            engine="evidently",
            drifted_columns=sorted(list(set(drifted_columns))),
            column_drift_scores=column_drift_scores
        )

        # Write clean summary JSON
        with open(json_path, "w", encoding="utf-8") as f:
            json.dump(drift_result.to_dict(), f, indent=2)

        # Write full Evidently metrics JSON for deep inspection
        full_json_path = json_path.replace(".json", "_full_evidently.json")
        try:
            with open(full_json_path, "w", encoding="utf-8") as f:
                json.dump(report_dict, f, indent=2)
        except Exception:
            pass

        logger.info(
            f"Evidently drift analysis complete: dataset_drift={dataset_drift_detected}, "
            f"share={drifted_share:.1%}, target_drift={target_drift_detected}, "
            f"prediction_drift={prediction_drift_detected}, concept_drift={concept_drift_detected}"
        )
        return drift_result
