"""
Base repository providing generic table lifecycle and high-speed PostgreSQL COPY operations.
"""
import io
import csv
import json
import pandas as pd
from typing import List, Optional
from sqlalchemy import text
from core.db.session import get_db_engine
from core.logging import get_logger

logger = get_logger(__name__)


class BaseRepository:
    """Base repository handling engine connection and low-level table operations."""

    VALID_TABLES = {
        "raw_black_friday",
        "black_friday_cleaned",
        "customer_segments",
        "product_network_metrics",
        "app_users",
        "user_purchases",
        "curated_products",
        "user_carts",
        "semantic_query_cache",
    }

    def __init__(self, engine=None):
        self.engine = engine or get_db_engine()

    def create_app_tables(self):
        """Creates all registered SQLAlchemy ORM database tables if they do not exist."""
        with self.engine.begin() as conn:
            if conn.dialect.name == "postgresql":
                conn.execute(text("CREATE EXTENSION IF NOT EXISTS vector;"))
        from core.db.models import Base
        Base.metadata.create_all(bind=self.engine)

    def truncate_table(self, table_name: str, restart_identity: bool = False):
        """Safely truncates an authorized warehouse table."""
        if table_name not in self.VALID_TABLES:
            raise ValueError(f"Unauthorized table truncation target: '{table_name}'")
        restart_clause = " RESTART IDENTITY" if restart_identity else ""
        logger.info(f"Truncating table '{table_name}'{restart_clause}...")
        with self.engine.begin() as conn:
            conn.execute(text(f"TRUNCATE TABLE {table_name}{restart_clause};"))

    def insert_dataframe(
        self,
        table_name: str,
        df: pd.DataFrame,
        if_exists: str = "append",
        chunksize: int = 50000,
        json_cols: Optional[List[str]] = None
    ):
        """High-performance bulk insertion using PostgreSQL native COPY streaming (100x faster than to_sql multi)."""
        if table_name not in self.VALID_TABLES:
            raise ValueError(f"Unauthorized table insertion target: '{table_name}'")

        data = df.copy()
        # Ensure integer columns that might be float with NaNs (e.g. product_category_2/3) format as whole integers or \N
        for col in ["product_category_2", "product_category_3", "occupation", "marital_status"]:
            if col in data.columns and pd.api.types.is_float_dtype(data[col]):
                data[col] = data[col].astype("Int64")

        if json_cols:
            for col in json_cols:
                if col in data.columns:
                    data[col] = data[col].apply(
                        lambda x: json.dumps(x) if isinstance(x, (list, dict)) else (x if isinstance(x, str) else json.dumps([]))
                    )

        logger.info(f"Writing {len(data):,} records to '{table_name}' (mode='{if_exists}')...")

        try:
            # High-speed COPY streaming path
            with self.engine.begin() as conn:
                if if_exists == "replace":
                    conn.execute(text(f"TRUNCATE TABLE {table_name} RESTART IDENTITY;"))

                dbapi_conn = conn.connection.dbapi_connection if hasattr(conn.connection, "dbapi_connection") else conn.connection
                buffer = io.StringIO()
                data.to_csv(buffer, index=False, header=False, sep="\t", na_rep="\\N", quoting=csv.QUOTE_MINIMAL)
                buffer.seek(0)

                columns_str = ", ".join([f'"{c}"' for c in data.columns])
                copy_sql = f"COPY {table_name} ({columns_str}) FROM STDIN WITH (FORMAT text, NULL '\\N')"

                with dbapi_conn.cursor() as cur:
                    cur.copy_expert(sql=copy_sql, file=buffer)

            logger.info(f"High-speed COPY streaming to '{table_name}' completed successfully.")
        except Exception as copy_err:
            logger.warning(f"Fast COPY failed ({copy_err}), falling back to standard to_sql batching...")
            if if_exists == "replace":
                self.truncate_table(table_name, restart_identity=True)
            data.to_sql(
                name=table_name,
                con=self.engine,
                if_exists="append",
                index=False,
                chunksize=chunksize,
                method="multi"
            )
            logger.info(f"Batch write to '{table_name}' complete.")
