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


def pytest_ignore_collect(collection_path, config):
    """`-m "not slow"` skips files that are slow as a whole, so their heavy imports (evidently, shap, mlflow, ...) never load."""
    if config.getoption("markexpr", default="") != "not slow" or collection_path.suffix != ".py":
        return None
    try:
        return "pytestmark = pytest.mark.slow" in collection_path.read_text(encoding="utf-8")
    except OSError:
        return None
