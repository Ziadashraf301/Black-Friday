"""Sliding-window rate limiter against the live Redis (DB 15)."""

import pytest

from apps.api.rate_limiting.rate_limiter import RedisRateLimiter
from core.exceptions import RateLimitExceededError


def test_redis_rate_limit_5_min_20_day():
    """Validates sliding window rate limiter enforcing 5 req/min and 20 req/day quotas."""
    test_user_id = "test_shopper_99999"
    daily_test_user = "test_shopper_daily_limit"
    limiter = RedisRateLimiter(minute_limit=5, day_limit=20)
    daily_limiter = RedisRateLimiter(minute_limit=100, day_limit=20)

    # Reset up front and always clean up the Redis keys, even when an assertion fails
    limiter.reset_user(test_user_id)
    daily_limiter.reset_user(daily_test_user)
    try:
        # 1. First 5 requests within a minute must pass
        for i in range(1, 6):
            allowed, msg, retry, headers = limiter.check_rate_limit(test_user_id)
            assert allowed is True, f"Request {i} should be allowed"
            assert headers.get("X-RateLimit-Limit-Minute") == "5"

        # 2. 6th request within the same minute must breach the quota (the API handler turns the error into a 429)
        allowed, msg, retry, headers = limiter.check_rate_limit(test_user_id)
        assert allowed is False, "6th request within minute must be blocked"
        assert "Max 5 requests per minute" in msg
        assert int(headers.get("Retry-After", "0")) > 0

        with pytest.raises(RateLimitExceededError) as exc_info:
            limiter.enforce(test_user_id)
        assert exc_info.value.code == "RATE_LIMIT_EXCEEDED"
        assert int(exc_info.value.headers.get("Retry-After", "0")) > 0

        # 3. Test daily limit (simulate 20 req/day quota)
        for i in range(1, 21):
            allowed, msg, retry, headers = daily_limiter.check_rate_limit(daily_test_user)
            assert allowed is True, f"Request {i} within day must be allowed"

        # 21st request exceeds daily quota
        allowed, msg, retry, headers = daily_limiter.check_rate_limit(daily_test_user)
        assert allowed is False, "21st request must exceed daily quota"
        assert "Max 20 requests per day" in msg
    finally:
        limiter.reset_user(test_user_id)
        daily_limiter.reset_user(daily_test_user)
