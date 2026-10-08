"""
Regression test for Fix 2.1:
- Non-destructive to_sql fallback using TRUNCATE + append preserving indexes
- Elimination of destructive float -> Int64 heuristic
- VALID_TABLES contains user_carts and semantic_query_cache
"""
import io
import uuid
from unittest.mock import patch
import pandas as pd
from sqlalchemy import create_engine, text, inspect
from core.config import settings
from core.db.repositories.base import BaseRepository


def test_valid_tables_includes_user_carts_and_semantic_cache():
    """Verify VALID_TABLES includes new application tables."""
    assert "user_carts" in BaseRepository.VALID_TABLES
    assert "semantic_query_cache" in BaseRepository.VALID_TABLES


def test_dataframe_with_whole_number_floats_preserves_dtype():
    """Verify float columns with whole numbers are NOT mutated to Int64 by insert_dataframe."""
    df = pd.DataFrame({
        "product_id": ["P001", "P002"],
        "predicted_usd": [100.0, 250.0],
        "category_float": [1.0, 2.0]
    })
    repo = BaseRepository()
    captured_data = []

    # Mock engine.begin() and cursor to intercept copy_expert
    with patch.object(repo.engine, "begin") as mock_begin:
        mock_conn = mock_begin.return_value.__enter__.return_value
        mock_dbapi = mock_conn.connection.dbapi_connection
        def fake_copy_expert(sql, file):
            captured_data.append(file.getvalue())
        mock_dbapi.cursor.return_value.__enter__.return_value.copy_expert.side_effect = fake_copy_expert

        repo.insert_dataframe("curated_products", df)

    # In old code: float columns were coerced to Int64, outputting "100" instead of "100.0"
    # In new code: float dtypes remain floats, outputting floating representation
    assert pd.api.types.is_float_dtype(df["predicted_usd"])
    assert pd.api.types.is_float_dtype(df["category_float"])
    assert len(captured_data) == 1
    assert "100.0\t1.0" in captured_data[0]


def test_copy_fallback_preserves_table_and_indexes():
    """Verify that when fast COPY fails with if_exists='replace', TRUNCATE + append preserves indexes."""
    scratch_db_name = f"test_copy_fallback_{uuid.uuid4().hex[:8]}"
    admin_engine = create_engine(settings.database_url, isolation_level="AUTOCOMMIT")
    scratch_engine = None

    try:
        with admin_engine.connect() as conn:
            conn.execute(text(f"CREATE DATABASE {scratch_db_name};"))

        scratch_url = settings.database_url.rsplit("/", 1)[0] + f"/{scratch_db_name}"
        scratch_engine = create_engine(scratch_url)

        # Create extension and a test table with an index
        with scratch_engine.begin() as conn:
            conn.execute(text("CREATE EXTENSION IF NOT EXISTS vector;"))
            conn.execute(text("""
                CREATE TABLE curated_products (
                    product_id VARCHAR(32) PRIMARY KEY,
                    name TEXT,
                    discounted_price FLOAT,
                    embedding vector(3)
                );
                CREATE INDEX idx_curated_embedding_hnsw 
                ON curated_products USING hnsw (embedding vector_cosine_ops);
            """))

        repo = BaseRepository(engine=scratch_engine)

        df = pd.DataFrame([
            {"product_id": "P001", "name": "Test Item 1", "discounted_price": 49.99},
            {"product_id": "P002", "name": "Test Item 2", "discounted_price": 89.99},
        ])

        # Force COPY path to fail by patching io.StringIO, triggering to_sql fallback
        with patch("core.db.repositories.base.io.StringIO", side_effect=RuntimeError("Simulated COPY error")):
            repo.insert_dataframe("curated_products", df, if_exists="replace")

        # Verify rows were inserted
        with scratch_engine.connect() as conn:
            count = conn.execute(text("SELECT COUNT(*) FROM curated_products;")).scalar()
            assert count == 2

        # Verify the HNSW index survived (was not dropped by to_sql replace)
        inspector = inspect(scratch_engine)
        indexes = [idx["name"] for idx in inspector.get_indexes("curated_products")]
        assert "idx_curated_embedding_hnsw" in indexes

    finally:
        if scratch_engine:
            scratch_engine.dispose()
        admin_engine.dispose()
        drop_engine = create_engine(settings.database_url, isolation_level="AUTOCOMMIT")
        with drop_engine.connect() as conn:
            conn.execute(text(f"DROP DATABASE IF EXISTS {scratch_db_name} WITH (FORCE);"))
        drop_engine.dispose()
