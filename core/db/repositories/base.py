"""
Base repository providing generic table lifecycle and high-speed PostgreSQL COPY operations.
"""
import io
import csv
import json
import pandas as pd
from typing import Dict, Any, List, Optional
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
        "user_purchases"
    }

    def __init__(self, engine=None):
        self.engine = engine or get_db_engine()

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
        if json_cols:
            for col in json_cols:
                if col in data.columns:
                    data[col] = data[col].apply(
                        lambda x: json.dumps(x) if isinstance(x, (list, dict)) else (x if isinstance(x, str) else json.dumps([]))
                    )

        # Convert float columns whose non-null values are whole numbers to nullable Int64
        for col in data.select_dtypes(include=["float", "float64"]).columns:
            non_null = data[col].dropna()
            if len(non_null) > 0 and (non_null % 1 == 0).all():
                data[col] = data[col].astype("Int64")

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
            data.to_sql(
                name=table_name,
                con=self.engine,
                if_exists=if_exists,
                index=False,
                chunksize=chunksize,
                method="multi"
            )
            logger.info(f"Batch write to '{table_name}' complete.")
