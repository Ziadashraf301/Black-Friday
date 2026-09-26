import os
import json

from core.db.repository import BlackFridayRepository
from ml.models.registry import ModelRegistry
from ml.models.evaluate import ModelEvaluator
from ml.models.onnx_exporter import ONNXExporter
from ml.tracking.mlflow_tracker import MLflowTracker
from ml.tracking.explainability import ModelExplainability
from ml.visualization.visualizer import Visualizer

from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

def run_training_pipeline(model_names: list = None):
    """Trains regression models, evaluates metrics, logs parameters, metrics, charts and ONNX models to MLflow,
    and registers the top model as Champion in MLflow Model Registry (SSOT).
    """
    repo = BlackFridayRepository()
    tracker = MLflowTracker()
    evaluator = ModelEvaluator(n_splits=10)

    logger.info("REGRESSION MODEL TRAINING & BENCHMARK PIPELINE")

    logger.info("Fetching cleaned non-outlier data from warehouse (using persisted Train/Test split)...")
    train_df = repo.get_cleaned_records_df(split="train", exclude_outliers=True)
    test_df = repo.get_cleaned_records_df(split="test", exclude_outliers=True)

    if train_df.empty or test_df.empty:
        raise ValueError(
            "Cleaned train/test partitions not found in database warehouse. "
            "Please run 'make preprocess' first to generate and persist the zero-leakage splits."
        )

    y_train = train_df["normalized_purchase"]
    y_test = test_df["normalized_purchase"]
    logger.info(f"Loaded Train split ({len(train_df):,} records) and Test split ({len(test_df):,} records).")

    # Dynamically load registered models from ModelRegistry (SSOT)
    available_model_names = ModelRegistry.list_available_models()
    if model_names:
        available_model_names = [m for m in model_names if m in available_model_names]
        if not available_model_names:
            raise ValueError(f"Requested models {model_names} not found. Available: {ModelRegistry.list_available_models()}")
    models = {name: ModelRegistry.get_model(name) for name in available_model_names}

    os.makedirs(os.path.join(settings.BASE_DIR, "models", "onnx"), exist_ok=True)
    metadata = {}
    model_artifacts_map = {}

    for model_name, model in models.items():
        logger.info(f"--- Training & Evaluating: {model_name} ---")

        # 1. 10-Fold Cross-Validation
        cv_metrics = evaluator.cross_validate(model, train_df, y_train)

        # 2. Fit on full training set
        model.fit(train_df, y_train)

        # 3. Evaluate on holdout test set
        test_preds = model.predict(test_df)
        holdout_metrics = evaluator.evaluate_holdout(y_test.to_numpy(), test_preds)

        all_metrics = {**cv_metrics, **{f"test_{k}": v for k, v in holdout_metrics.items()}}
        logger.info(f"{model_name} Metrics -> Test R2: {all_metrics.get('test_r2', 0):.4f}, Test RMSE: {all_metrics.get('test_rmse', 0):.4f}")

        # 4. Generate Model Evaluation Charts
        artifacts = Visualizer.generate_model_evaluation_charts(
            y_true=y_test.to_numpy(),
            y_pred=test_preds,
            model_name=model_name,
            output_dir=f"reports/models/{model_name}"
        )

        # 5. Export to ONNX & Verify Parity
        onnx_file = os.path.join(settings.BASE_DIR, "models", "onnx", f"{model_name}.onnx")
        ONNXExporter.export_regression_pipeline(
            pipeline=model.pipeline,
            feature_names=model.features,
            output_filepath=onnx_file
        )
        sample_subset = test_df[model.features].head(50)
        ONNXExporter.verify_parity(model, onnx_file, sample_subset)

        # 6. SHAP Explainability for all models (Explainability Engine + Visualizer)
        logger.info(f"Computing SHAP feature attributions for {model_name}...")
        bg_sample = train_df[model.features].head(100)
        explainer = ModelExplainability(model.pipeline, background_sample=bg_sample)
        shap_values, X_eval = explainer.explain(test_df[model.features].head(200))
        if shap_values is not None:
            shap_plot_path = Visualizer.generate_shap_summary_plot(
                shap_values=shap_values,
                eval_features=X_eval,
                output_dir=f"reports/shap_{model_name}"
            )
            if shap_plot_path:
                artifacts["shap_summary_plot"] = shap_plot_path

        model_artifacts_map[model_name] = (artifacts, onnx_file)
        metadata[model_name] = {
            "metrics": all_metrics,
            "params": model.get_params(),
            "onnx_path": onnx_file,
            "purchase_max": settings.PURCHASE_MAX,
        }

    # 7. Generate Overall Model Benchmark Comparison Chart
    benchmark_chart = Visualizer.generate_benchmark_comparison_chart(metadata)

    # 8. Log Runs to MLflow
    for model_name, model in models.items():
        artifacts, onnx_file = model_artifacts_map[model_name]
        artifacts["benchmark_comparison_chart"] = benchmark_chart

        run_id = tracker.log_training_run(
            model_name=model_name,
            params={
                **model.get_params(),
                "normalization_purchase_max": settings.PURCHASE_MAX,
                "cv_folds": 10,
                "train_records": len(train_df),
                "test_records": len(test_df)
            },
            metrics=metadata[model_name]["metrics"],
            artifacts=artifacts,
            onnx_model_path=onnx_file
        )
        metadata[model_name]["run_id"] = run_id

    # 9. Identify and Promote Best Model as Initial Champion in MLflow Model Registry (SSOT)
    best_model_name = max(metadata, key=lambda m: metadata[m]["metrics"].get("test_r2", 0.0))
    best_run_id = metadata[best_model_name].get("run_id")

    if best_run_id:
        logger.info(f"🏆 Promoting best model '{best_model_name}' (Test R2: {metadata[best_model_name]['metrics']['test_r2']:.4f}) to Champion in MLflow Model Registry...")
        tracker.promote_to_champion(run_id=best_run_id, model_name=settings.MLFLOW_MODEL_NAME)
        tracker.download_champion_onnx(model_name=settings.MLFLOW_MODEL_NAME)

    # Save local metadata cache
    metadata_path = os.path.join(settings.BASE_DIR, "models", "metadata.json")
    with open(metadata_path, "w", encoding="utf-8") as f:
        json.dump(metadata, f, indent=2)

    logger.info(f"Training pipeline complete. Champion registered in MLflow: '{best_model_name}'.")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Black Friday Regression Model Training & Benchmark Suite")
    parser.add_argument(
        "--model", "-m",
        type=str,
        default=None,
        help="Train a specific model (e.g. lgbm, random_forest, linear_regression, decision_tree). Trains all if omitted."
    )
    args = parser.parse_args()
    run_training_pipeline(model_names=[args.model] if args.model else None)
