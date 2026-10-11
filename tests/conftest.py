"""
Fixtures shared by every test folder.

Test isolation: all runs target the dedicated Redis DB 15 (never DB 0). The per-test cleanup of Redis and
PostgreSQL lives in tests/integration/conftest.py and tests/e2e/conftest.py, so tests/unit never opens a connection.
"""
import os

# Enforce REDIS_DB=15 before core.config is imported anywhere
os.environ["REDIS_DB"] = "15"

from core.config import settings  # noqa: E402

settings.REDIS_DB = 15
