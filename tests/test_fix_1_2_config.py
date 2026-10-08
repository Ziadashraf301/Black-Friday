"""
Regression tests for Fix 1.2:
- GEMINI_API_KEY is optional (defaults to None when unset)
- Database and Redis passwords with special characters (@, :, /, #, ?) are quote_plus encoded
  and parse back accurately to the raw password
- Consolidated validator parses empty/whitespace/valid integer values correctly
"""
import os
import urllib.parse
from core.config import Settings


def test_gemini_api_key_optional(monkeypatch):
    """Verify Settings initializes cleanly when GEMINI_API_KEY is not set in environment."""
    monkeypatch.delenv("GEMINI_API_KEY", raising=False)
    s = Settings(GEMINI_API_KEY=None)
    assert s.GEMINI_API_KEY is None


def test_special_characters_in_db_and_redis_passwords():
    """Verify passwords containing @, :, /, #, ? and space are URL-encoded with quote(safe='') and parse back accurately."""
    import sqlalchemy.engine
    import redis

    special_password = "p@ss:w/o#r?d with spaces"

    s = Settings(
        POSTGRES_USER="testuser",
        POSTGRES_PASSWORD=special_password,
        POSTGRES_HOST="localhost",
        POSTGRES_PORT=5432,
        POSTGRES_DB="testdb",
        REDIS_PASSWORD=special_password,
        REDIS_HOST="localhost",
        REDIS_PORT=6379,
        REDIS_DB=0,
    )

    # 1. Sync Postgres URL
    pg_url = s.database_url
    assert "%20" in pg_url or "p%40ss" in pg_url
    parsed_pg_pw = sqlalchemy.engine.make_url(pg_url).password
    assert parsed_pg_pw == special_password

    # 2. Redis URL
    redis_url = s.redis_url
    r_client = redis.from_url(redis_url)
    parsed_redis_pw = r_client.connection_pool.connection_kwargs.get("password")
    assert parsed_redis_pw == special_password


def test_consolidated_optional_integer_validator():
    """Verify consolidated validator parses optional integers or empty strings to None/int."""
    s = Settings(
        DT_MAX_DEPTH="",
        RF_MAX_DEPTH="none",
        IMPUTER_SAMPLE_SIZE=None,
        IMPUTER_MAX_DEPTH="null",
    )
    assert s.DT_MAX_DEPTH is None
    assert s.RF_MAX_DEPTH is None
    assert s.IMPUTER_SAMPLE_SIZE is None
    assert s.IMPUTER_MAX_DEPTH is None

    s2 = Settings(
        DT_MAX_DEPTH="20",
        RF_MAX_DEPTH=15,
        IMPUTER_SAMPLE_SIZE="50000",
        IMPUTER_MAX_DEPTH=10,
    )
    assert s2.DT_MAX_DEPTH == 20
    assert s2.RF_MAX_DEPTH == 15
    assert s2.IMPUTER_SAMPLE_SIZE == 50000
    assert s2.IMPUTER_MAX_DEPTH == 10

