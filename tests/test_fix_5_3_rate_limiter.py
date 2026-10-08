import time
import pytest
from apps.api.rate_limiting.rate_limiter import RedisRateLimiter, LUA_SLIDING_WINDOW_RATE_LIMIT
from fastapi import HTTPException


def test_lua_script_defined():
    assert "ZREMRANGEBYSCORE" in LUA_SLIDING_WINDOW_RATE_LIMIT
    assert "ZCARD" in LUA_SLIDING_WINDOW_RATE_LIMIT
    assert "ZADD" in LUA_SLIDING_WINDOW_RATE_LIMIT


def test_in_memory_rate_limiter_allows_and_blocks():
    # Force in-memory by passing redis_client=None
    limiter = RedisRateLimiter(minute_limit=3, day_limit=10)
    # mock client property to return None
    limiter._custom_client = None
    limiter._in_memory_windows = {}
    
    # First 3 requests allowed
    allowed, _, _, _ = limiter._check_in_memory("test-client", time.time())
    assert allowed is True
    allowed, _, _, _ = limiter._check_in_memory("test-client", time.time())
    assert allowed is True
    allowed, _, _, _ = limiter._check_in_memory("test-client", time.time())
    assert allowed is True
    
    # 4th request blocked
    allowed, msg, _, headers = limiter._check_in_memory("test-client", time.time())
    assert allowed is False
    assert "Rate limit exceeded" in msg
    assert headers["Retry-After"] is not None


def test_in_memory_rate_limiter_bounded_cleanup():
    limiter = RedisRateLimiter(minute_limit=10, day_limit=100)
    now = time.time()
    
    # Insert older timestamps (> 60 seconds ago)
    past_time = now - 70.0
    limiter._in_memory_windows["mem:min:old-client"] = [past_time]
    
    # Call cleanup
    limiter._clean_expired_in_memory(now)
    
    # Expired client entries should be cleaned up
    assert "mem:min:old-client" not in limiter._in_memory_windows


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
