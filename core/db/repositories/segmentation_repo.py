"""
Segmentation repository for customer personas and clusters.
"""
import pandas as pd
from typing import Dict, Any, List, Optional
from sqlalchemy import text
from core.db.repositories.base import BaseRepository
from core.logging import get_logger

logger = get_logger(__name__)


class SegmentationRepository(BaseRepository):
    """Repository managing customer persona clusters and segmentation tables."""

    def truncate_customer_segments(self):
        self.truncate_table("customer_segments")

    def insert_customer_segments(self, df: pd.DataFrame):
        self.insert_dataframe("customer_segments", df, if_exists="replace", chunksize=5000)

    def get_customer_segment(self, user_id: int) -> Optional[Dict[str, Any]]:
        """Retrieves customer persona profile for a specific user ID."""
        query = text("SELECT * FROM customer_segments WHERE user_id = :user_id")
        with self.engine.connect() as conn:
            result = conn.execute(query, {"user_id": user_id}).mappings().first()
            return dict(result) if result else None

    def get_all_personas_summary(self) -> List[Dict[str, Any]]:
        """Returns aggregate metrics for all 10 customer personas."""
        query = text("""
            SELECT 
                cluster_id,
                cluster_persona,
                COUNT(*) as customer_count,
                ROUND(AVG(lifetime_value), 2) as avg_lifetime_value,
                ROUND(AVG(frequency), 1) as avg_frequency,
                ROUND(AVG(average_order_value), 2) as avg_aov,
                recommended_action
            FROM customer_segments
            GROUP BY cluster_id, cluster_persona, recommended_action
            ORDER BY cluster_id ASC
        """)
        with self.engine.connect() as conn:
            return [dict(row) for row in conn.execute(query).mappings().all()]
