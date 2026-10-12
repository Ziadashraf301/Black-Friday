"""Rate limiter Lua script executed by the live Redis."""
import time

import pytest
from fastapi import HTTPException
from fastapi.testclient import TestClient

from apps.api.main import app
from apps.api.rate_limiting.rate_limiter import LUA_SLIDING_WINDOW_RATE_LIMIT, RedisRateLimiter, auth_rate_limiter
from core.config import settings


def test_lua_script_real_execution_integration():
    """Verify Lua script executes against real Redis without syntax/runtime errors."""
    import redis

    from core.config import settings
    try:
        r = redis.Redis(
            host=settings.REDIS_HOST,
            port=settings.REDIS_PORT,
            password=settings.REDIS_PASSWORD or None,
            db=15,
            socket_timeout=1.0,
        )
        r.ping()
    except Exception:
        pytest.skip("Local Redis server not reachable for Lua script integration test.")

    test_uid = f"lua-test-{int(time.time())}"
    now_ts = time.time()
    limiter = RedisRateLimiter(redis_client=r, minute_limit=5, day_limit=20)

    try:
        # Run rate limit check via Redis Lua script
        allowed, msg, retry_after, headers = limiter.check_rate_limit(test_uid)
        assert allowed is True
        assert headers["X-RateLimit-Limit-Minute"] == "5"

        # Check Redis keys created
        keys = r.keys(f"ratelimit:*:{test_uid}")
        assert len(keys) >= 1
    finally:
        for k in r.keys(f"ratelimit:*:{test_uid}"):
            r.delete(k)
