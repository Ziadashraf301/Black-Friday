import os
import shutil
try:
    import mlflow
    from mlflow.tracking import MlflowClient
    HAS_MLFLOW = True
except ImportError:
    HAS_MLFLOW = False
    class MlflowClient:
        def __init__(self, *args, **kwargs):
            pass

from typing import Dict, Any, Optional, List
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

class MLflowTracker:
    """Enterprise MLflow client managing experiments, metrics, artifacts, Model Registry, and Champion governance."""

    def __init__(self, experiment_name: str = settings.MLFLOW_EXPERIMENT_NAME):
        self.tracking_uri = settings.MLFLOW_TRACKING_URI
        self.experiment_name = experiment_name
        self.client: Optional[MlflowClient] = None
        self._setup_mlflow()

    def _setup_mlflow(self):
        """Initializes MLflow tracking URI, experiment, and MlflowClient."""
        try:
            # Set S3/MinIO environment configuration for boto3 artifact store
            os.environ.setdefault("MLFLOW_S3_ENDPOINT_URL", "http://localhost:9000")
            os.environ.setdefault("AWS_ACCESS_KEY_ID", settings.MINIO_ROOT_USER)
            os.environ.setdefault("AWS_SECRET_ACCESS_KEY", settings.MINIO_ROOT_PASSWORD)

            mlflow.set_tracking_uri(self.tracking_uri)
            mlflow.set_experiment(self.experiment_name)
            self.client = MlflowClient(tracking_uri=self.tracking_uri)
            logger.info(f"MLflow client connected to: {self.tracking_uri} (Experiment: '{self.experiment_name}')")
        except Exception as e:
            logger.warning(f"Could not connect to MLflow tracking server at {self.tracking_uri}: {e}")
            self.client = None

    def log_run(
        self,
        run_name: str,
        params: Dict[str, Any],
        metrics: Dict[str, float],
        tags: Optional[Dict[str, str]] = None,
        artifacts: Optional[Dict[str, str]] = None,
        onnx_model_path: Optional[str] = None
    ) -> Optional[str]:
        """Logs parameters, metrics, artifacts, and ONNX model to active MLflow experiment."""
        try:
            with mlflow.start_run(run_name=run_name) as run:
                if tags:
                    mlflow.set_tags(tags)
                mlflow.log_params(params)
                mlflow.log_metrics(metrics)

                if artifacts:
                    for art_name, art_path in artifacts.items():
                        if os.path.exists(art_path):
                            if os.path.isdir(art_path):
                                mlflow.log_artifacts(art_path, artifact_path=art_name)
                            else:
                                mlflow.log_artifact(art_path)

                if onnx_model_path and os.path.exists(onnx_model_path):
                    mlflow.log_artifact(onnx_model_path, artifact_path="onnx_models")

                run_id = run.info.run_id
                logger.info(f"Successfully logged run '{run_name}' to MLflow. Run ID: {run_id}")
                return run_id
        except Exception as e:
            logger.error(f"Failed to log run to MLflow: {e}")
            return None

    def get_champion_metrics(self, model_name: str = settings.MLFLOW_MODEL_NAME) -> Dict[str, Any]:
        """Retrieves performance metrics and parameters of the current active Champion model from MLflow.
        
        Queries the MLflow Model Registry for versions in 'Production' or alias 'champion'.
        Falls back to searching the experiment for the best historical promoted run.
        """
        if not self.client:
            logger.warning("MLflow client unavailable. Unable to query champion metrics.")
            return {"r2": 0.0, "rmse": float("inf"), "version": None, "run_id": None}

        try:
            # 1. Query Model Registry for Production version or @champion alias
            try:
                prod_versions = self.client.get_latest_versions(model_name, stages=["Production"])
                if prod_versions:
                    latest_prod = prod_versions[0]
                    run = self.client.get_run(latest_prod.run_id)
                    metrics = run.data.metrics
                    logger.info(f"Found active Champion in Model Registry (v{latest_prod.version}, Run: {latest_prod.run_id}).")
                    return {
                        "r2": float(metrics.get("test_r2", metrics.get("cv_mean_r2", 0.0))),
                        "rmse": float(metrics.get("test_rmse", metrics.get("cv_mean_rmse", float("inf")))),
                        "version": latest_prod.version,
                        "run_id": latest_prod.run_id,
                        "model_name": latest_prod.name,
                        "params": run.data.params,
                        "metrics": metrics
                    }
            except Exception as reg_err:
                logger.debug(f"No registered model '{model_name}' found in Production: {reg_err}")

            # 2. Fallback: Query experiment runs for highest test_r2 with PROMOTED tag
            experiment = self.client.get_experiment_by_name(self.experiment_name)
            if experiment:
                runs = self.client.search_runs(
                    experiment_ids=[experiment.experiment_id],
                    filter_string="tags.deployment_status = 'PROMOTED_TO_PRODUCTION' or tags.deployment_status = 'CHAMPION'",
                    order_by=["metrics.test_r2 DESC"],
                    max_results=1
                )
                if runs:
                    best_run = runs[0]
                    metrics = best_run.data.metrics
                    logger.info(f"Found historical Champion run from experiment search: {best_run.info.run_id}")
                    return {
                        "r2": float(metrics.get("test_r2", 0.0)),
                        "rmse": float(metrics.get("test_rmse", float("inf"))),
                        "version": None,
                        "run_id": best_run.info.run_id,
                        "model_name": best_run.data.tags.get("model_name", "champion"),
                        "params": best_run.data.params,
                        "metrics": metrics
                    }

            return {"r2": 0.0, "rmse": float("inf"), "version": None, "run_id": None}
        except Exception as e:
            logger.error(f"Error querying champion model from MLflow: {e}")
            return {"r2": 0.0, "rmse": float("inf"), "version": None, "run_id": None}

    def register_and_promote_model(
        self,
        run_id: str,
        model_name: str,
        artifact_subpath: str,
        description: str = "Production model promoted in MLflow Model Registry.",
        stage: str = "Production",
        aliases: Optional[List[str]] = None,
        deployment_status: str = "PROMOTED_TO_PRODUCTION"
    ) -> Optional[Dict[str, Any]]:
        """Registers a run artifact in MLflow Model Registry and promotes it with aliases (Single Source of Truth)."""
        if not self.client:
            logger.warning("MLflow client unavailable. Skipping Model Registry promotion.")
            return None

        aliases = aliases or ["production", "champion"]
        try:
            # 1. Create registered model if not present
            try:
                self.client.create_registered_model(model_name)
                logger.info(f"Created registered model '{model_name}' in MLflow Model Registry.")
            except Exception:
                pass  # Model already registered

            # 2. Create new Model Version from run artifact
            model_source = f"runs:/{run_id}/{artifact_subpath}"
            version = self.client.create_model_version(
                name=model_name,
                source=model_source,
                run_id=run_id,
                description=description
            )
            logger.info(f"Created model version v{version.version} for '{model_name}'.")

            # 3. Transition to target stage
            self.client.transition_model_version_stage(
                name=model_name,
                version=version.version,
                stage=stage,
                archive_existing_versions=True
            )

            # 4. Set first-class Model Registry aliases
            for alias in aliases:
                try:
                    self.client.set_registered_model_alias(model_name, alias, str(version.version))
                    logger.info(f"Assigned alias '@{alias}' to '{model_name}' v{version.version}.")
                except Exception as alias_err:
                    logger.debug(f"Could not set alias '{alias}' on '{model_name}': {alias_err}")

            # 5. Tag run and model version
            self.client.set_tag(run_id, "deployment_status", deployment_status)
            self.client.set_tag(run_id, "model_version", str(version.version))
            self.client.set_model_version_tag(
                name=model_name,
                version=version.version,
                key="deployment_status",
                value=deployment_status
            )

            logger.info(f"Model version v{version.version} successfully promoted in '{model_name}' to {stage} with aliases {aliases}.")
            return {"version": version.version, "model_name": model_name, "stage": stage}
        except Exception as e:
            logger.error(f"Failed to register and promote model '{model_name}' in MLflow: {e}", exc_info=True)
            return None

    def promote_to_champion(
        self,
        run_id: str,
        model_name: str = settings.MLFLOW_MODEL_NAME,
        onnx_artifact_subpath: str = "onnx_models"
    ) -> Optional[Dict[str, Any]]:
        """Registers a candidate model in MLflow Model Registry and transitions it to 'Production' Champion."""
        return self.register_and_promote_model(
            run_id=run_id,
            model_name=model_name,
            artifact_subpath=onnx_artifact_subpath,
            description="Promoted Champion model via automated retrain pipeline.",
            stage="Production",
            aliases=["production", "champion"],
            deployment_status="CHAMPION"
        )

    def download_champion_onnx(
        self,
        model_name: str = settings.MLFLOW_MODEL_NAME,
        target_dir: str = "models/onnx"
    ) -> Optional[str]:
        """Downloads the production champion ONNX model from MLflow storage directly to local serving cache."""
        try:
            model_uri = f"models:/{model_name}/Production"
            logger.info(f"Downloading production model artifact from MLflow URI: '{model_uri}'...")
            downloaded_path = mlflow.artifacts.download_artifacts(artifact_uri=model_uri, dst_path=target_dir)
            logger.info(f"Champion artifact downloaded to: {downloaded_path}")
            return downloaded_path
        except Exception as e:
            logger.warning(f"Could not download champion model from MLflow: {e}")
            return None

    def get_all_models_metrics(self) -> List[Dict[str, Any]]:
        """Fetches all model evaluation metrics from MLflow runs in the active experiment (SSOT)."""
        if not self.client:
            return []

        try:
            experiment = self.client.get_experiment_by_name(self.experiment_name)
            if not experiment:
                return []

            runs = self.client.search_runs(
                experiment_ids=[experiment.experiment_id],
                order_by=["start_time DESC"],
                max_results=50
            )

            results = []
            seen_models = set()
            for r in runs:
                # Use the explicit model_name tag (set at log time), not run-name string parsing
                name = r.data.tags.get("model_name") or r.data.tags.get("model_architecture") or r.data.tags.get("mlflow.runName", r.info.run_id)
                clean_name = name.replace("_", " ").title()
                if clean_name not in seen_models and "test_rmse" in r.data.metrics:
                    seen_models.add(clean_name)
                    results.append({
                        "model_name": clean_name,
                        "test_rmse": round(float(r.data.metrics.get("test_rmse", 0.0)), 4),
                        "test_r2": round(float(r.data.metrics.get("test_r2", 0.0)), 4),
                        "cv_mean_rmse": round(float(r.data.metrics.get("cv_mean_rmse", r.data.metrics.get("test_rmse", 0.0))), 4),
                        "cv_mean_r2": round(float(r.data.metrics.get("cv_mean_r2", r.data.metrics.get("test_r2", 0.0))), 4),
                        "run_id": r.info.run_id,
                        "status": r.data.tags.get("deployment_status", "EVALUATED")
                    })
            return results
        except Exception as e:
            logger.error(f"Error fetching model metrics from MLflow: {e}")
            return []

    def log_training_run(
        self,
        model_name: str,
        params: Dict[str, Any],
        metrics: Dict[str, float],
        artifacts: Optional[Dict[str, str]] = None,
        onnx_model_path: Optional[str] = None
    ) -> Optional[str]:
        """High-level abstraction for logging initial model training runs."""
        return self.log_run(
            run_name=model_name,
            params=params,
            metrics=metrics,
            tags={
                "model_name": model_name,
                "model_architecture": model_name,
                "pipeline_stage": "initial_training"
            },
            artifacts=artifacts,
            onnx_model_path=onnx_model_path
        )

    def log_retrain_run(
        self,
        model_name: str,
        trigger_reason: str,
        is_promoted: bool,
        params: Dict[str, Any],
        metrics: Dict[str, float],
        tags: Dict[str, str],
        artifacts: Optional[Dict[str, str]] = None,
        onnx_model_path: Optional[str] = None
    ) -> Optional[str]:
        """High-level abstraction for logging Champion vs Challenger retraining runs."""
        status_prefix = "champion" if is_promoted else "challenger_rejected"
        return self.log_run(
            run_name=f"{status_prefix}_{model_name}_{trigger_reason}",
            params=params,
            metrics=metrics,
            tags=tags,
            artifacts=artifacts,
            onnx_model_path=onnx_model_path if is_promoted else None
        )

    def log_drift_run(
        self,
        model_name: str,
        params: Dict[str, Any],
        metrics: Dict[str, float],
        tags: Dict[str, str],
        artifacts: Optional[Dict[str, str]] = None
    ) -> Optional[str]:
        """High-level abstraction for logging production data drift monitoring runs."""
        return self.log_run(
            run_name=f"drift_monitor_{model_name}",
            params=params,
            metrics=metrics,
            tags=tags,
            artifacts=artifacts
        )

    def log_preprocessing_run(
        self,
        params: Dict[str, Any],
        metrics: Dict[str, float],
        tags: Dict[str, str],
        artifacts: Optional[Dict[str, str]] = None,
        imputer_model_path: Optional[str] = None,
        register_imputer_name: Optional[str] = settings.MLFLOW_IMPUTER_MODEL_NAME
    ) -> Optional[str]:
        """High-level abstraction for logging preprocessing and MissForest imputation runs."""
        run_artifacts = dict(artifacts or {})
        if imputer_model_path and os.path.exists(imputer_model_path):
            run_artifacts["imputer_model"] = imputer_model_path

        run_id = self.log_run(
            run_name="missforest_imputation_and_normalization",
            params=params,
            metrics=metrics,
            tags=tags,
            artifacts=run_artifacts
        )

        if run_id and register_imputer_name and imputer_model_path and self.client:
            self.register_and_promote_model(
                run_id=run_id,
                model_name=register_imputer_name,
                artifact_subpath="imputer_model",
                description="Production MissForest Imputer ONNX models fitted on train transactions.",
                stage="Production",
                aliases=["production", "champion"],
                deployment_status="PROMOTED_TO_PRODUCTION"
            )

        return run_id
