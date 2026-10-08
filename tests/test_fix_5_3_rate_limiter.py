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
