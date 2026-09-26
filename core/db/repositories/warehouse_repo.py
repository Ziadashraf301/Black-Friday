"""
Warehouse repository for transaction ingestion and cleaned data warehouse queries.
"""
import pandas as pd
from typing import Dict, Optional
from sqlalchemy import text
from core.db.repositories.base import BaseRepository
from core.logging import get_logger

logger = get_logger(__name__)


class WarehouseRepository(BaseRepository):
    """Repository managing raw_black_friday and black_friday_cleaned tables."""

    def truncate_raw_table(self):
        self.truncate_table("raw_black_friday", restart_identity=True)

    def insert_raw_batch(self, df: pd.DataFrame, chunksize: int = 25000):
        self.insert_dataframe("raw_black_friday", df, if_exists="append", chunksize=chunksize)

    def truncate_cleaned_table(self):
        self.truncate_table("black_friday_cleaned", restart_identity=True)

    def insert_cleaned_batch(self, df: pd.DataFrame, chunksize: int = 25000):
        self.insert_dataframe("black_friday_cleaned", df, if_exists="append", chunksize=chunksize)

    def get_raw_records_df(self, limit: Optional[int] = None) -> pd.DataFrame:
        """Retrieves raw ingested records."""
        if limit:
            query = text("SELECT * FROM raw_black_friday LIMIT :limit")
            params = {"limit": int(limit)}
        else:
            query = text("SELECT * FROM raw_black_friday")
            params = {}
        with self.engine.connect() as conn:
            return pd.read_sql(query, conn, params=params)

    def get_cleaned_records_df(
        self,
        exclude_outliers: bool = False,
        split: Optional[str] = None,
        limit: Optional[int] = None
    ) -> pd.DataFrame:
        """Retrieves cleaned and imputed records."""
        query = "SELECT user_id, product_id, gender, age, occupation, city_category, " \
                "stay_in_current_city_years, marital_status, product_category_1, " \
                "product_category_2, product_category_3, purchase, is_outlier, normalized_purchase, split " \
                "FROM black_friday_cleaned"
        conditions = []
        params = {}
        if exclude_outliers:
            conditions.append("is_outlier = FALSE")
        if split:
            conditions.append("split = :split")
            params["split"] = split
        if conditions:
            query += " WHERE " + " AND ".join(conditions)
        if limit:
            query += f" LIMIT {int(limit)}"
        with self.engine.connect() as conn:
            return pd.read_sql(text(query), conn, params=params)

    def get_product_order_counts(self) -> Dict[str, int]:
        """Returns total order count for each product from cleaned transactions."""
        query = text("SELECT product_id, COUNT(*) as order_count FROM black_friday_cleaned GROUP BY product_id")
        with self.engine.connect() as conn:
            return {row["product_id"]: int(row["order_count"]) for row in conn.execute(query).mappings().all()}
