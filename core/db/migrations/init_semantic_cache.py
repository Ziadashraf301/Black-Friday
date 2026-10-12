"""
Initializes semantic_query_cache table and HNSW pgvector index for true semantic caching.
"""

from sqlalchemy import text

from core.db.session import get_db_session
from core.logging import get_logger

logger = get_logger(__name__)


def init_semantic_cache_table():
    with get_db_session() as session:
        session.execute(
            text(
                """
            CREATE EXTENSION IF NOT EXISTS vector;

            CREATE TABLE IF NOT EXISTS semantic_query_cache (
                id BIGSERIAL PRIMARY KEY,
                query_text TEXT NOT NULL,
                embedding vector(768) NOT NULL,
                response_text TEXT NOT NULL,
                ui_payload JSONB NOT NULL,
                intent VARCHAR(64) NOT NULL,
                user_key TEXT,
                created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
            );

            CREATE INDEX IF NOT EXISTS idx_semantic_cache_hnsw
            ON semantic_query_cache USING hnsw (embedding vector_cosine_ops);

            ALTER TABLE semantic_query_cache ADD COLUMN IF NOT EXISTS user_key TEXT;
            CREATE INDEX IF NOT EXISTS idx_semantic_cache_user_created
            ON semantic_query_cache (user_key, created_at);
        """
            )
        )
        session.commit()
        logger.info("[DB: MIGRATION] semantic_query_cache table and HNSW index verified.")


if __name__ == "__main__":
    init_semantic_cache_table()
