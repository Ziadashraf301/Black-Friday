"""
Analytics repository for executive KPI queries, demographic distributions, and statistical tests.
"""
from typing import Dict, Any, List
from sqlalchemy import text
from core.db.repositories.base import BaseRepository
from core.logging import get_logger

logger = get_logger(__name__)


class AnalyticsRepository(BaseRepository):
    """Repository managing executive analytics, aggregations, and demographic stats."""

    def get_eda_summary(self) -> Dict[str, Any]:
        """Calculates executive KPI totals from the cleaned data."""
        query = text("""
            SELECT 
                COUNT(*) as total_orders,
                COUNT(DISTINCT user_id) as total_users,
                COUNT(DISTINCT product_id) as total_products,
                ROUND(AVG(purchase), 2) as avg_order_value,
                ROUND(SUM(purchase), 2) as total_revenue
            FROM black_friday_cleaned
        """)
        with self.engine.connect() as conn:
            result = conn.execute(query).mappings().first()
            res = dict(result) if result else {}
            res = {k: (float(v) if hasattr(v, "as_tuple") else v) for k, v in res.items()}
            return res

    def get_demographic_distribution(self, column_name: str) -> List[Dict[str, Any]]:
        """Distribution of orders across specified demographic column."""
        allowed_columns = {"gender", "age", "marital_status", "city_category", "occupation", "stay_in_current_city_years"}
        if column_name not in allowed_columns:
            raise ValueError(f"Column '{column_name}' is not an authorized categorical feature.")
        query = text(f"""
            SELECT {column_name} as category, COUNT(*) as order_count,
                   ROUND(AVG(purchase), 2) as avg_purchase,
                   ROUND(SUM(purchase), 2) as total_purchase
            FROM black_friday_cleaned
            GROUP BY {column_name}
            ORDER BY order_count DESC
        """)
        with self.engine.connect() as conn:
            return [dict(row) for row in conn.execute(query).mappings().all()]


