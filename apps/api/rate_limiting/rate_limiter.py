"""
Redis Sliding-Window Rate Limiter (Task P1-05).
Enforces:
  - Max 5 requests / minute per authenticated user_id
  - Max 20 requests / day per authenticated user_id
Returns HTTP 429 Too Many Requests on breach.
Follows SOLID principles and Controller-Service-Repository architecture.
"""
import time
import uuid
from typing import Dict, Any, Optional, Tuple
from fastapi import HTTPException, status, Request, Depends
import redis

from core.cache import cache_manager
from core.config import settings
from core.logging import get_logger
from apps.api.auth import get_current_user

logger = get_logger(__name__)


class RedisRateLimiter:
    """
    Sliding window rate limiter backed by Redis sorted sets (ZSET).
    Enforces per-minute and per-day request limits per authenticated user.
    """

    def __init__(
        self,
        minute_limit: int = 5,
        day_limit: int = 20,
        redis_client: Optional[redis.Redis] = None,
    ):
        self.minute_limit = minute_limit
        self.day_limit = day_limit
        self._in_memory_windows: Dict[str, list] = {}
        self._custom_client = redis_client

    @property
    def client(self) -> Optional[redis.Redis]:
        if self._custom_client:
            return self._custom_client
        if cache_manager.is_available:
            return cache_manager._client
        return None

    def _check_in_memory(self, user_id: str, now: float) -> Tuple[bool, Optional[str], int]:
        """Fallback in-memory sliding window when Redis is offline."""
        min_key = f"mem:min:{user_id}"
        day_key = f"mem:day:{user_id}"

        # Minute window
        timestamps = [t for t in self._in_memory_windows.get(min_key, []) if t > now - 60.0]
        if len(timestamps) >= self.minute_limit:
            retry_after = int(60.0 - (now - timestamps[0])) + 1
            return False, f"Rate limit exceeded: Max {self.minute_limit} requests per minute.", retry_after

        # Day window
        day_timestamps = [t for t in self._in_memory_windows.get(day_key, []) if t > now - 86400.0]
        if len(day_timestamps) >= self.day_limit:
            retry_after = int(86400.0 - (now - day_timestamps[0])) + 1
            return False, f"Rate limit exceeded: Max {self.day_limit} requests per day.", retry_after

        timestamps.append(now)
        day_timestamps.append(now)
        self._in_memory_windows[min_key] = timestamps
        self._in_memory_windows[day_key] = day_timestamps
        return True, None, 0

    def check_rate_limit(self, user_id: str) -> Tuple[bool, Optional[str], int, Dict[str, str]]:
        """
        Evaluates sliding window rate limits for user_id.
        Returns: (is_allowed, error_message, retry_after_seconds, headers)
        """
        now = time.time()
        r = self.client

        if r is None:
            allowed, msg, retry = self._check_in_memory(str(user_id), now)
            headers = {"Retry-After": str(retry)} if not allowed else {}
            return allowed, msg, retry, headers

        min_key = f"ratelimit:minute:{user_id}"
        day_key = f"ratelimit:day:{user_id}"

        pipe = r.pipeline()
        # 1. Prune expired entries
        pipe.zremrangebyscore(min_key, 0, now - 60.0)
        pipe.zcard(min_key)
        pipe.zremrangebyscore(day_key, 0, now - 86400.0)
        pipe.zcard(day_key)
        results = pipe.execute()

        minute_count = results[1]
        day_count = results[3]

        # Check minute quota
        if minute_count >= self.minute_limit:
            oldest = r.zrange(min_key, 0, 0, withscores=True)
            oldest_ts = oldest[0][1] if oldest else now - 60.0
            retry_after = max(1, int(60.0 - (now - oldest_ts)))
            headers = {
                "Retry-After": str(retry_after),
                "X-RateLimit-Limit-Minute": str(self.minute_limit),
                "X-RateLimit-Remaining-Minute": "0",
            }
            return False, f"Rate limit exceeded: Max {self.minute_limit} requests per minute.", retry_after, headers

        # Check daily quota
        if day_count >= self.day_limit:
            oldest = r.zrange(day_key, 0, 0, withscores=True)
            oldest_ts = oldest[0][1] if oldest else now - 86400.0
            retry_after = max(1, int(86400.0 - (now - oldest_ts)))
            headers = {
                "Retry-After": str(retry_after),
                "X-RateLimit-Limit-Day": str(self.day_limit),
                "X-RateLimit-Remaining-Day": "0",
            }
            return False, f"Rate limit exceeded: Max {self.day_limit} requests per day.", retry_after, headers

        # Record this request
        member = f"{now}:{uuid.uuid4().hex[:8]}"
        pipe = r.pipeline()
        pipe.zadd(min_key, {member: now})
        pipe.expire(min_key, 65)
        pipe.zadd(day_key, {member: now})
        pipe.expire(day_key, 86500)
        pipe.execute()

        headers = {
            "X-RateLimit-Limit-Minute": str(self.minute_limit),
            "X-RateLimit-Remaining-Minute": str(max(0, self.minute_limit - (minute_count + 1))),
            "X-RateLimit-Limit-Day": str(self.day_limit),
            "X-RateLimit-Remaining-Day": str(max(0, self.day_limit - (day_count + 1))),
        }
        return True, None, 0, headers

    def enforce(self, user_id: str):
        """Raises HTTP 429 Too Many Requests if rate limits are breached."""
        allowed, msg, retry_after, headers = self.check_rate_limit(user_id)
        if not allowed:
            raise HTTPException(
                status_code=status.HTTP_429_TOO_MANY_REQUESTS,
                detail=msg,
                headers=headers,
            )

    def reset_user(self, user_id: str):
        """Resets rate limit tracking keys for user_id (useful in tests)."""
        r = self.client
        if r is not None:
            r.delete(f"ratelimit:minute:{user_id}", f"ratelimit:day:{user_id}")
        self._in_memory_windows.pop(f"mem:min:{user_id}", None)
        self._in_memory_windows.pop(f"mem:day:{user_id}", None)


# Default singleton instance
rate_limiter = RedisRateLimiter(minute_limit=5, day_limit=20)


async def rate_limit_dependency(
    current_user: Dict[str, Any] = Depends(get_current_user),
) -> Dict[str, Any]:
    """FastAPI route dependency enforcing rate limits for the authenticated user."""
    user_id = str(current_user.get("user_id", "anonymous"))
    rate_limiter.enforce(user_id)
    return current_user
