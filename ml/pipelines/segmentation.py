import os
import json
from core.db.repository import BlackFridayRepository
from ml.features.customer_features import CustomerFeatureExtractor
from ml.segmentation.clustering import CustomerSegmentationEngine, PERSONA_PROFILES
from ml.tracking.mlflow_tracker import MLflowTracker
from ml.visualization.visualizer import Visualizer
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


def run_customer_segmentation():
    """Extracts customer features, performs Gower hierarchical clustering, logs telemetry to MLflow, and stores persona profiles."""
    repo = BlackFridayRepository()
    tracker = MLflowTracker()

    logger.info("Reading cleaned transactions for customer profiling...")
    transactions_df = repo.get_cleaned_records_df(exclude_outliers=False)
    if transactions_df.empty:
        raise ValueError("No cleaned transactions found in black_friday_cleaned.")

    # 1. Feature Extraction
    extractor = CustomerFeatureExtractor()
    customer_df = extractor.extract_features(transactions_df)

    # 2. Gower Distance Hierarchical Clustering
    engine = CustomerSegmentationEngine()
    segmented_df = engine.fit_predict(customer_df)

    # 3. Compute Per-Cluster Statistics (Mean of numeric features, Mode of categorical features)
    stats_df = engine.compute_cluster_statistics(segmented_df)

    # 4. Persist to customer_segments table in PostgreSQL warehouse
    repo.insert_customer_segments(segmented_df)

    # 5. Generate Summary Metrics and Artifacts
    persona_counts = segmented_df["cluster_persona"].value_counts().to_dict()
    metrics = {
        "total_unique_customers": float(len(segmented_df)),
        "transactions_analyzed": float(len(transactions_df)),
        "k_clusters": float(settings.K_CLUSTERS),
    }
    for persona_name, count in persona_counts.items():
        clean_key = persona_name.lower().replace(" ", "_").replace("<=", "le").replace(">", "gt").replace("(", "").replace(")", "")
        metrics[f"persona_count_{clean_key}"] = float(count)

    # Add numeric cluster means to metrics for MLflow tracking
    for _, row in stats_df.iterrows():
        cid = row["cluster_id"]
        if "mean_lifetime_value" in row:
            metrics[f"cluster_{cid}_mean_ltv"] = float(row["mean_lifetime_value"])
        if "mean_frequency" in row:
            metrics[f"cluster_{cid}_mean_freq"] = float(row["mean_frequency"])
        if "mean_average_order_value" in row:
            metrics[f"cluster_{cid}_mean_aov"] = float(row["mean_average_order_value"])

    summary_dir = os.path.join(settings.BASE_DIR, "reports", "segmentation")
    os.makedirs(summary_dir, exist_ok=True)
    summary_path = os.path.join(summary_dir, "segmentation_summary.json")

    # Save cluster statistics as CSV and JSON
    stats_csv_path = os.path.join(summary_dir, "cluster_statistics.csv")
    stats_json_path = os.path.join(summary_dir, "cluster_statistics.json")
    stats_df.to_csv(stats_csv_path, index=False)
    stats_df.to_json(stats_json_path, orient="records", indent=2)

    persona_details = []
    for cid, info in PERSONA_PROFILES.items():
        subset = segmented_df[segmented_df["cluster_id"] == cid]
        persona_details.append({
            "cluster_id": cid,
            "persona_name": info["persona"],
            "recommended_action": info["action"],
            "customer_count": len(subset),
            "customer_share": round(len(subset) / max(len(segmented_df), 1), 4)
        })

    with open(summary_path, "w", encoding="utf-8") as f:
        json.dump({
            "params": {
                "k_clusters": settings.K_CLUSTERS,
                "clustering_algorithm": "Hierarchical Complete Linkage over Gower Dissimilarity Matrix",
                "feature_columns": engine.feature_cols
            },
            "metrics": metrics,
            "persona_breakdown": persona_details,
            "cluster_statistics": stats_df.to_dict(orient="records")
        }, f, indent=2)

    # 6. Generate Diagnostic Visual Charts (including cluster statistics table chart)
    charts = Visualizer.generate_segmentation_charts(segmented_df, stats_df=stats_df, output_dir=summary_dir)

    artifacts = {
        "segmentation_summary": summary_path,
        "cluster_statistics_csv": stats_csv_path,
        "cluster_statistics_json": stats_json_path,
        **charts
    }

    # 7. Log Run to MLflow
    run_id = tracker.log_run(
        run_name="customer_segmentation_gower",
        params={
            "k_clusters": settings.K_CLUSTERS,
            "distance_metric": settings.SEGMENTATION_METRIC,
            "linkage": settings.SEGMENTATION_LINKAGE,
            "feature_columns": ",".join(engine.feature_cols)
        },
        metrics=metrics,
        tags={
            "pipeline_stage": "customer_segmentation",
            "algorithm": "Gower_Hierarchical_Clustering",
            "persona_count": str(settings.K_CLUSTERS)
        },
        artifacts=artifacts
    )

    logger.info(f"Customer segmentation pipeline finished successfully. MLflow Run ID: {run_id}")
    return segmented_df


if __name__ == "__main__":
    run_customer_segmentation()
