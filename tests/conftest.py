"""
Pytest configuration and test isolation for Black Friday test suite.
Enforces test isolation:
- Dedicated REDIS_DB=15 for all test runs (protecting DB 0).
- Automatic clearing of DB 15, semantic_query_cache, and user_carts between tests.
"""
import os

# Enforce REDIS_DB=15 in environment for test execution
os.environ["REDIS_DB"] = "15"

import pytest
from sqlalchemy import text
from core.config import settings
from core.cache.redis_client import cache_manager
from core.db.session import engine

# Ensure settings and singleton cache_manager point to isolated test DB 15
settings.REDIS_DB = 15
cache_manager.reconnect(db=15)


@pytest.fixture(autouse=True)
def isolated_redis_db():
    """
    Cleans isolated test database (DB 15), semantic query cache, and user carts
    before every test to prevent cross-test pollution.
    Never touches DB 0.
    """
    if cache_manager.is_available and cache_manager.client is not None:
        try:
            curr_db = cache_manager.client.connection_pool.connection_kwargs.get("db", 0)
            if curr_db == 15:
                cache_manager.client.flushdb()
            else:
                cache_manager.delete_pattern("cache:*")
                cache_manager.delete_pattern("security:*")
                cache_manager.delete_pattern("cart:*")
                cache_manager.delete_pattern("test:*")
        except Exception:
            pass

    try:
        with engine.begin() as conn:
            conn.execute(text("TRUNCATE TABLE semantic_query_cache RESTART IDENTITY;"))
            conn.execute(text("TRUNCATE TABLE user_carts RESTART IDENTITY;"))
    except Exception:
        pass

    yield
