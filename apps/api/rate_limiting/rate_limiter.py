"""
Redis Sliding-Window Rate Limiter (Task P1-05, Fix 5.3).
Enforces:
  - Max requests / minute per user or client
  - Max requests / day per user or client
Uses atomic Redis Lua script evaluation for strict concurrency safety.
Thread-safe, bounded memory fallback when Redis is offline.
"""
import time
import uuid
import threading
from typing import Dict, Any, Optional, Tuple, List
from fastapi import HTTPException, status, Request, Depends
import redis

from core.cache import cache_manager
from core.config import settings
from core.logging import get_logger
from apps.api.auth import get_current_user, decode_access_token

logger = get_logger(__name__)

LUA_SLIDING_WINDOW_RATE_LIMIT = """
local min_key = KEYS[1]
local day_key = KEYS[2]
local now = tonumber(ARGV[1])
local min_window = tonumber(ARGV[2])
local day_window = tonumber(ARGV[3])
local min_limit = tonumber(ARGV[4])
local day_limit = tonumber(ARGV[5])
local member = ARGV[6]

-- 1. Prune expired entries
redis.call('ZREMRANGEBYSCORE', min_key, '-inf', now - min_window)
redis.call('ZREMRANGEBYSCORE', day_key, '-inf', now - day_window)

-- 2. Count current entries in windows
local min_count = redis.call('ZCARD', min_key)
local day_count = redis.call('ZCARD', day_key)

-- 3. Check minute limit
if min_count >= min_limit then
    local oldest = redis.call('ZRANGE', min_key, 0, 0, 'WITHSCORES')
    local oldest_ts = (oldest and #oldest >= 2) and tonumber(oldest[2]) or (now - min_window)
    local retry_after = math.max(1, math.ceil(min_window - (now - oldest_ts)))
    return {0, 'minute', retry_after, min_count, day_count}
end

-- 4. Check day limit
if day_count >= day_limit then
    local oldest = redis.call('ZRANGE', day_key, 0, 0, 'WITHSCORES')
    local oldest_ts = (oldest and #oldest >= 2) and tonumber(oldest[2]) or (now - day_window)
    local retry_after = math.max(1, math.ceil(day_window - (now - oldest_ts)))
    return {0, 'day', retry_after, min_count, day_count}
end

-- 5. Atomic record: add new entry and set TTLs
redis.call('ZADD', min_key, now, member)
redis.call('EXPIRE', min_key, math.ceil(min_window + 5))
redis.call('ZADD', day_key, now, member)
redis.call('EXPIRE', day_key, math.ceil(day_window + 100))

return {1, 'ok', 0, min_count + 1, day_count + 1}
"""


class RedisRateLimiter:
    """
    Sliding window rate limiter backed by atomic Redis sorted sets (ZSET).
    Enforces per-minute and per-day request limits per user or client identifier.
    """

    def __init__(
        self,
        minute_limit: Optional[int] = None,
        day_limit: Optional[int] = None,
        redis_client: Optional[redis.Redis] = None,
    ):
        self.minute_limit = minute_limit if minute_limit is not None else getattr(settings, "RATE_LIMIT_MINUTE", 60)
        self.day_limit = day_limit if day_limit is not None else getattr(settings, "RATE_LIMIT_DAY", 1000)
        self._in_memory_windows: Dict[str, List[float]] = {}
        self._lock = threading.Lock()
        self._custom_client = redis_client

    @property
    def client(self) -> Optional[redis.Redis]:
        if self._custom_client is not None:
            return self._custom_client
        if cache_manager.is_available:
            return cache_manager.client
        return None

    def _clean_expired_in_memory(self, now: float) -> None:
        """Internal helper running under self._lock to prune expired in-memory entries."""
        empty_keys = []
        for key, timestamps in self._in_memory_windows.items():
            window = 60.0 if ":min:" in key else 86400.0
            valid = [t for t in timestamps if t > now - window]
            if not valid:
                empty_keys.append(key)
            else:
                self._in_memory_windows[key] = valid
        for key in empty_keys:
            self._in_memory_windows.pop(key, None)

    def _check_in_memory(self, user_id: str, now: float) -> Tuple[bool, Optional[str], int, Dict[str, str]]:
        """Thread-safe bounded in-memory sliding window fallback when Redis is offline."""
        with self._lock:
            # Sweep if dict grows
            if len(self._in_memory_windows) > 200:
                self._clean_expired_in_memory(now)

            min_key = f"mem:min:{user_id}"
            day_key = f"mem:day:{user_id}"

            min_timestamps = [t for t in self._in_memory_windows.get(min_key, []) if t > now - 60.0]
            day_timestamps = [t for t in self._in_memory_windows.get(day_key, []) if t > now - 86400.0]

            # Clean empty keys
            if not min_timestamps and min_key in self._in_memory_windows:
                del self._in_memory_windows[min_key]
            if not day_timestamps and day_key in self._in_memory_windows:
                del self._in_memory_windows[day_key]

            # Check minute limit
            if len(min_timestamps) >= self.minute_limit:
                retry_after = max(1, int(60.0 - (now - min_timestamps[0])))
                headers = {
                    "Retry-After": str(retry_after),
                    "X-RateLimit-Limit-Minute": str(self.minute_limit),
                    "X-RateLimit-Remaining-Minute": "0",
                }
                return False, f"Rate limit exceeded: Max {self.minute_limit} requests per minute.", retry_after, headers

            # Check day limit
            if len(day_timestamps) >= self.day_limit:
                retry_after = max(1, int(86400.0 - (now - day_timestamps[0])))
                headers = {
                    "Retry-After": str(retry_after),
                    "X-RateLimit-Limit-Day": str(self.day_limit),
                    "X-RateLimit-Remaining-Day": "0",
                }
                return False, f"Rate limit exceeded: Max {self.day_limit} requests per day.", retry_after, headers

            # Allowed: record timestamp
            min_timestamps.append(now)
            day_timestamps.append(now)
            self._in_memory_windows[min_key] = min_timestamps
            self._in_memory_windows[day_key] = day_timestamps

            headers = {
                "X-RateLimit-Limit-Minute": str(self.minute_limit),
                "X-RateLimit-Remaining-Minute": str(max(0, self.minute_limit - len(min_timestamps))),
                "X-RateLimit-Limit-Day": str(self.day_limit),
                "X-RateLimit-Remaining-Day": str(max(0, self.day_limit - len(day_timestamps))),
            }
            return True, None, 0, headers

    def check_rate_limit(self, user_id: str) -> Tuple[bool, Optional[str], int, Dict[str, str]]:
        """
        Evaluates sliding window rate limits for user_id atomically.
        Returns: (is_allowed, error_message, retry_after_seconds, headers)
        """
        now = time.time()
        r = self.client

        if r is None:
            return self._check_in_memory(str(user_id), now)

        min_key = f"ratelimit:minute:{user_id}"
        day_key = f"ratelimit:day:{user_id}"
        member = f"{now}:{uuid.uuid4().hex[:8]}"

        try:
            res = r.eval(
                LUA_SLIDING_WINDOW_RATE_LIMIT,
                2,
                min_key,
                day_key,
                now,
                60.0,
                86400.0,
                self.minute_limit,
                self.day_limit,
                member,
            )
            is_allowed = bool(res[0] == 1)
            reason = res[1].decode("utf-8") if isinstance(res[1], bytes) else str(res[1])
            retry_after = int(res[2])
            min_count = int(res[3])
            day_count = int(res[4])

            if not is_allowed:
                window_name = "minute" if reason == "minute" else "day"
                limit_val = self.minute_limit if window_name == "minute" else self.day_limit
                msg = f"Rate limit exceeded: Max {limit_val} requests per {window_name}."
                headers = {
                    "Retry-After": str(retry_after),
                    f"X-RateLimit-Limit-{window_name.capitalize()}": str(limit_val),
                    f"X-RateLimit-Remaining-{window_name.capitalize()}": "0",
                }
                return False, msg, retry_after, headers

            headers = {
                "X-RateLimit-Limit-Minute": str(self.minute_limit),
                "X-RateLimit-Remaining-Minute": str(max(0, self.minute_limit - min_count)),
                "X-RateLimit-Limit-Day": str(self.day_limit),
                "X-RateLimit-Remaining-Day": str(max(0, self.day_limit - day_count)),
            }
            return True, None, 0, headers

        except Exception as e:
            logger.warning(f"Redis rate limiter Lua execution failed, falling back to in-memory: {e}")
            return self._check_in_memory(str(user_id), now)

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
            try:
                r.delete(f"ratelimit:minute:{user_id}", f"ratelimit:day:{user_id}")
            except Exception:
                pass
        with self._lock:
            self._in_memory_windows.pop(f"mem:min:{user_id}", None)
            self._in_memory_windows.pop(f"mem:day:{user_id}", None)


# Default singleton instance configured with sane limits for normal test & UI traffic
rate_limiter = RedisRateLimiter(
    minute_limit=getattr(settings, "RATE_LIMIT_MINUTE", 60),
    day_limit=getattr(settings, "RATE_LIMIT_DAY", 1000),
)


async def rate_limit_dependency(
    current_user: Dict[str, Any] = Depends(get_current_user),
) -> Dict[str, Any]:
    """FastAPI route dependency enforcing rate limits for the authenticated user."""
    user_id = str(current_user.get("user_id", "anonymous"))
    rate_limiter.enforce(user_id)
    return current_user


def extract_client_identifier(request: Request) -> str:
    """Extracts user ID from Bearer JWT, X-User-ID header, query parameter, or client IP."""
    auth_header = request.headers.get("Authorization")
    if auth_header and auth_header.startswith("Bearer "):
        try:
            token = auth_header.split(" ", 1)[1].strip()
            payload = decode_access_token(token)
            if payload.get("sub"):
                return str(payload.get("sub"))
        except Exception:
            pass
    return (
        request.headers.get("X-User-ID")
        or request.query_params.get("user_id")
        or (request.client.host if request.client else "anonymous")
    )


async def rate_limit_client_dependency(request: Request):
    """FastAPI route dependency enforcing rate limits on anonymous/guest or authenticated caller."""
    identifier = extract_client_identifier(request)
    rate_limiter.enforce(identifier)
