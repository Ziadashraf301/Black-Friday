"""
Fixtures for the e2e tests, which use the live services.

Automatic clearing of Redis DB 15, semantic_query_cache and user_carts before every test.
"""
from pathlib import Path

import pytest
from sqlalchemy import text

from core.cache.redis_client import cache_manager
from core.db.session import engine
from tests.helpers import mark_folder

# Point the singleton cache_manager at the isolated test DB 15
cache_manager.reconnect(db=15)


def pytest_collection_modifyitems(config, items):
    mark_folder(items, Path(__file__).parent, "e2e")


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
