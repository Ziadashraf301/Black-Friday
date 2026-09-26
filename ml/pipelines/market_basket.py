import os
import json
import pandas as pd
from core.db.repository import BlackFridayRepository
from ml.features.basket_encoder import TransactionBasketEncoder
from ml.market_basket.apriori_engine import AprioriAssociationEngine
from ml.market_basket.network_graph import ProductNetworkGraph
from ml.market_basket.item2vec import Item2VecRecommender
from ml.tracking.mlflow_tracker import MLflowTracker
from ml.visualization.visualizer import Visualizer
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


def run_market_basket_pipeline(graph_source: str = "auto"):
    """Extracts baskets, mines Apriori rules & Item2Vec embeddings, computes graph centrality, logs telemetry to MLflow, and stores metrics.

    Args:
        graph_source: 'apriori', 'item2vec', or 'auto' (prefers apriori, falls back to item2vec).
    """
    repo = BlackFridayRepository()
    tracker = MLflowTracker()

    logger.info("Fetching cleaned transactions for Market Basket analysis...")
    transactions_df = repo.get_cleaned_records_df(exclude_outliers=False)
    if transactions_df.empty:
        raise ValueError("black_friday_cleaned table is empty.")

    # 1. Encode user baskets
    encoder = TransactionBasketEncoder()
    baskets = encoder.build_baskets(transactions_df)
    basket_matrix = encoder.fit_transform(baskets)

    # 2. Mine Apriori Rules using Settings hyperparams
    min_support = settings.APRIORI_MIN_SUPPORT
    min_confidence = settings.APRIORI_MIN_CONFIDENCE
    apriori_engine = AprioriAssociationEngine(min_support=min_support, min_confidence=min_confidence)
    rules_df = apriori_engine.mine_rules(basket_matrix)

    # 3. Train Item2Vec dense embeddings using Settings hyperparams
    vector_size = settings.ITEM2VEC_VECTOR_SIZE
    window = settings.ITEM2VEC_WINDOW
    min_count = settings.ITEM2VEC_MIN_COUNT
    epochs = settings.ITEM2VEC_EPOCHS
    item2vec = Item2VecRecommender(vector_size=vector_size, window=window, min_count=min_count, epochs=epochs)
    item2vec.fit(baskets)

    # Evaluate Item2Vec embedding quality and alignment with Apriori rules
    item2vec_metrics = item2vec.evaluate_embeddings(baskets=baskets, rules_df=rules_df)

    # 4. Construct Unified Graph & Centrality Scoring (Apriori + Item2Vec)
    network = ProductNetworkGraph()
    order_counts = repo.get_product_order_counts()
    metrics_df = network.build_unified_metrics(rules_df, item2vec, order_counts=order_counts)

    # 5. Persist to product_network_metrics table in database warehouse
    if not metrics_df.empty:
        repo.insert_product_network_metrics(metrics_df)

    # 6. Generate Summary Artifacts, CSV Reports, and Diagnostic Visual Charts
    reports_dir = os.path.join(settings.BASE_DIR, "reports", "market_basket")
    os.makedirs(reports_dir, exist_ok=True)

    rules_csv_path = os.path.join(reports_dir, "apriori_rules.csv")
    if not rules_df.empty:
        rules_df.to_csv(rules_csv_path, index=False)

    # Export Item2Vec recommendations CSV
    i2v_csv_path = os.path.join(reports_dir, "item2vec_recommendations.csv")
    all_recs = item2vec.get_all_similar_products(top_n=5)
    rec_rows = []
    for pid, sim_list in all_recs.items():
        for rec_pid, sim_score in sim_list:
            rec_rows.append({"source_product_id": pid, "recommended_product_id": rec_pid, "cosine_similarity": sim_score})
    if rec_rows:
        pd.DataFrame(rec_rows).to_csv(i2v_csv_path, index=False)

    summary_json_path = os.path.join(reports_dir, "market_basket_summary.json")
    summary_data = {
        "params": {
            "min_support": min_support,
            "min_confidence": min_confidence,
            "vector_size": vector_size,
            "window": window,
            "min_count": min_count,
            "epochs": epochs,
            "graph_source": graph_source
        },
        "metrics": {
            "total_baskets": len(baskets),
            "total_transactions": len(transactions_df),
            "apriori_rules_mined": len(rules_df),
            "avg_rule_support": round(float(rules_df["support"].mean()), 4) if not rules_df.empty else 0.0,
            "avg_rule_confidence": round(float(rules_df["confidence"].mean()), 4) if not rules_df.empty else 0.0,
            "avg_rule_lift": round(float(rules_df["lift"].mean()), 4) if not rules_df.empty else 0.0,
            "product_network_nodes": len(metrics_df),
            **item2vec_metrics
        }
    }
    with open(summary_json_path, "w", encoding="utf-8") as f:
        json.dump(summary_data, f, indent=2)

    charts = Visualizer.generate_market_basket_charts(rules_df, metrics_df, item2vec=item2vec, output_dir=reports_dir)

    # 7. Log Run to MLflow
    artifacts = {
        "market_basket_summary": summary_json_path,
        **charts
    }
    if os.path.exists(rules_csv_path):
        artifacts["apriori_rules_csv"] = rules_csv_path
    if os.path.exists(i2v_csv_path):
        artifacts["item2vec_recommendations_csv"] = i2v_csv_path

    run_id = tracker.log_run(
        run_name="market_basket_product_network",
        params=summary_data["params"],
        metrics=summary_data["metrics"],
        tags={
            "pipeline_stage": "market_basket_network",
            "algorithm": "Apriori_Item2Vec_NetworkGraph",
            "graph_source": graph_source
        },
        artifacts=artifacts
    )

    logger.info(f"Market basket and product network analysis pipeline complete. MLflow Run ID: {run_id}")
    return metrics_df


if __name__ == "__main__":
    run_market_basket_pipeline()
