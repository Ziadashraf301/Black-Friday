"""
Recommendation repository for product network metrics, PageRank centrality, and bundle associations.
"""
import pandas as pd
from typing import Dict, Any, List, Optional
from sqlalchemy import text, bindparam
from core.db.repositories.base import BaseRepository
from core.logging import get_logger

logger = get_logger(__name__)


class RecommendationRepository(BaseRepository):
    """Repository managing product_network_metrics and recommendation queries."""

    def truncate_product_network_metrics(self):
        self.truncate_table("product_network_metrics")

    def insert_product_network_metrics(self, df: pd.DataFrame):
        self.insert_dataframe(
            "product_network_metrics",
            df,
            if_exists="replace",
            chunksize=5000,
            json_cols=["top_bundle_recommendations", "item2vec_recommendations"]
        )

    def get_top_network_products(self, limit: int = 15) -> List[Dict[str, Any]]:
        """Retrieves top products ranked by PageRank, Hubs, and Authority."""
        query = text("""
            SELECT product_id, order_count, pagerank_score, hub_score, authority_score, 
                   top_associated_product, highest_lift_rule,
                   top_bundle_recommendations, item2vec_recommendations
            FROM product_network_metrics
            ORDER BY pagerank_score DESC
            LIMIT :limit
        """)
        with self.engine.connect() as conn:
            return [dict(row) for row in conn.execute(query, {"limit": limit}).mappings().all()]

    def get_product_recommendations(self, product_id: str) -> Optional[Dict[str, Any]]:
        """Retrieves both Apriori bundle rules and Item2Vec recommendations for a product."""
        query = text("""
            SELECT product_id, order_count, pagerank_score, hub_score, authority_score,
                   top_associated_product, highest_lift_rule,
                   top_bundle_recommendations, item2vec_recommendations
            FROM product_network_metrics
            WHERE product_id = :product_id
        """)
        with self.engine.connect() as conn:
            result = conn.execute(query, {"product_id": product_id}).mappings().first()
            return dict(result) if result else None

    def get_product_categories(self, product_id: str, session: Optional[Any] = None) -> Optional[Dict[str, Any]]:
        """
        Returns the most-common product_category_1, _2, _3 for a product_id.
        Used for ONNX inference when shopper hasn't supplied category values.
        Utilizes index on product_id in black_friday_cleaned.
        """
        query = text("""
            SELECT
                MODE() WITHIN GROUP (ORDER BY product_category_1) AS product_category_1,
                MODE() WITHIN GROUP (ORDER BY product_category_2) AS product_category_2,
                MODE() WITHIN GROUP (ORDER BY product_category_3) AS product_category_3
            FROM black_friday_cleaned
            WHERE product_id = :pid
        """)
        if session is not None:
            result = session.execute(query, {"pid": product_id}).mappings().first()
            return dict(result) if result else None

        with self.engine.connect() as conn:
            result = conn.execute(query, {"pid": product_id}).mappings().first()
            return dict(result) if result else None

    def get_bulk_product_categories(
        self, product_ids: List[str], session: Optional[Any] = None
    ) -> Dict[str, Dict[str, Any]]:
        """
        Returns the most-common product_category_1, _2, _3 for a list of product_ids in ONE query.
        Eliminates N+1 category queries during batch pricing.
        """
        clean_pids = list({pid for pid in product_ids if pid})
        if not clean_pids:
            return {}

        query = text("""
            SELECT
                product_id,
                MODE() WITHIN GROUP (ORDER BY product_category_1) AS product_category_1,
                MODE() WITHIN GROUP (ORDER BY product_category_2) AS product_category_2,
                MODE() WITHIN GROUP (ORDER BY product_category_3) AS product_category_3
            FROM black_friday_cleaned
            WHERE product_id IN :pids
            GROUP BY product_id
        """).bindparams(bindparam("pids", expanding=True))

        if session is not None:
            results = session.execute(query, {"pids": clean_pids}).mappings().all()
        else:
            with self.engine.connect() as conn:
                results = conn.execute(query, {"pids": clean_pids}).mappings().all()

        return {row["product_id"]: dict(row) for row in results}

