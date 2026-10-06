"""
Rate Limiter Service wrapping rate limiter core for the service layer.
"""
from typing import Dict, Any, Tuple, Optional
from apps.api.rate_limiting.rate_limiter import rate_limiter, RedisRateLimiter


class RateLimiterService:
    """Service layer interface for rate limiting."""

    def __init__(self, limiter: Optional[RedisRateLimiter] = None):
        self.limiter = limiter or rate_limiter

    def check_user(self, user_id: str) -> Tuple[bool, Optional[str], int, Dict[str, str]]:
        return self.limiter.check_rate_limit(str(user_id))

    def enforce_user(self, user_id: str) -> None:
        self.limiter.enforce(str(user_id))

    def reset(self, user_id: str) -> None:
        self.limiter.reset_user(str(user_id))


rate_limiter_service = RateLimiterService()
