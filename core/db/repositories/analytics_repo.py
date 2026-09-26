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
        """Calculates executive KPI totals from the cleaned data with in-memory caching."""
        if hasattr(self, "_cache_eda_summary"):
            return self._cache_eda_summary
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
            if res:
                self._cache_eda_summary = res
            return res

    def get_top_users_by_orders(self, limit: int = 10) -> List[Dict[str, Any]]:
        """Top users sorted by total transaction count."""
        query = text("""
            SELECT user_id, COUNT(*) as num_orders, ROUND(SUM(purchase), 2) as total_spend
            FROM black_friday_cleaned
            GROUP BY user_id
            ORDER BY num_orders DESC
            LIMIT :limit
        """)
        with self.engine.connect() as conn:
            return [dict(row) for row in conn.execute(query, {"limit": limit}).mappings().all()]

    def get_top_users_by_spend(self, limit: int = 10) -> List[Dict[str, Any]]:
        """Top users sorted by total expenditure in USD."""
        query = text("""
            SELECT user_id, COUNT(*) as num_orders, ROUND(SUM(purchase), 2) as total_spend
            FROM black_friday_cleaned
            GROUP BY user_id
            ORDER BY total_spend DESC
            LIMIT :limit
        """)
        with self.engine.connect() as conn:
            return [dict(row) for row in conn.execute(query, {"limit": limit}).mappings().all()]

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

    def get_dimension_stats(self, dimension: str) -> Dict[str, Dict[str, float]]:
        """
        SQL-aggregated count, mean, and sample variance for any of the 5 EDA dimensions.
        Returns: { category_label -> {count, mean, std, var} }
        """
        allowed = {"gender", "age", "marital_status", "city_category", "occupation"}
        if dimension not in allowed:
            raise ValueError(f"Dimension '{dimension}' is not supported. Must be one of {allowed}.")

        query = text(f"""
            SELECT {dimension} as grp,
                   COUNT(*) as n,
                   AVG(purchase) as mean_usd,
                   VAR_SAMP(purchase) as var_usd
            FROM black_friday_cleaned
            GROUP BY {dimension}
            ORDER BY {dimension}
        """)

        with self.engine.connect() as conn:
            rows = conn.execute(query).mappings().all()

        result: Dict[str, Dict[str, float]] = {}
        for r in rows:
            grp = str(r["grp"])
            if dimension == "gender":
                grp = "male" if grp == "M" else ("female" if grp == "F" else grp)
            elif dimension == "marital_status":
                grp = "Single" if str(grp) in ("0", "Single") else "Married"

            var_val = float(r["var_usd"]) if r["var_usd"] is not None else 0.0
            result[grp] = {
                "count": int(r["n"]),
                "mean": float(r["mean_usd"]),
                "std": var_val ** 0.5,
                "var": var_val,
            }
        return result

    def get_dimension_distribution(self, dimension: str) -> List[Dict]:
        """Category-level order counts, avg purchase and total purchase for EDA bar charts."""
        allowed = {"gender", "age", "marital_status", "city_category", "occupation"}
        if dimension not in allowed:
            raise ValueError(f"Unsupported dimension: {dimension}")

        query = text(f"""
            SELECT {dimension} as category,
                   COUNT(*) as order_count,
                   ROUND(AVG(purchase), 2) as avg_purchase,
                   ROUND(SUM(purchase), 2) as total_purchase
            FROM black_friday_cleaned
            GROUP BY {dimension}
            ORDER BY {dimension}
        """)
        with self.engine.connect() as conn:
            return [dict(r) for r in conn.execute(query).mappings().all()]
