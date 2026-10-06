"""
Initializes semantic_query_cache table and HNSW pgvector index for true semantic caching.
"""
from core.db.session import get_db_session
from sqlalchemy import text
from core.logging import get_logger

logger = get_logger(__name__)


def init_semantic_cache_table():
    with get_db_session() as session:
        session.execute(text("""
            CREATE EXTENSION IF NOT EXISTS vector;

            CREATE TABLE IF NOT EXISTS semantic_query_cache (
                id BIGSERIAL PRIMARY KEY,
                query_text TEXT NOT NULL,
                embedding vector(768) NOT NULL,
                response_text TEXT NOT NULL,
                ui_payload JSONB NOT NULL,
                intent VARCHAR(64) NOT NULL,
                created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
            );

            CREATE INDEX IF NOT EXISTS idx_semantic_cache_hnsw 
            ON semantic_query_cache USING hnsw (embedding vector_cosine_ops);
        """))
        session.commit()
        logger.info("[DB: MIGRATION] semantic_query_cache table and HNSW index verified.")


if __name__ == "__main__":
    init_semantic_cache_table()
