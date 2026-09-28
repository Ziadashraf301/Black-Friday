import json
import decimal
import datetime
from typing import Optional, Any, List, Dict
import redis
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


def _json_serializer(obj: Any) -> Any:
    """Helper serializer for Decimal, date/datetime, and numpy types for JSON encoding."""
    if isinstance(obj, decimal.Decimal):
        return float(obj)
    if isinstance(obj, (datetime.date, datetime.datetime)):
        return obj.isoformat()
    if hasattr(obj, "item"):  # handles numpy scalar types
        return obj.item()
    return str(obj)


class RedisCacheManager:
    """High-performance Redis persistent caching client with fallback capabilities."""

    def __init__(self):
        self._client: Optional[redis.Redis] = None
        self._is_available = False
        self._connect()

    def _connect(self):
        try:
            self._client = redis.Redis(
                host=settings.REDIS_HOST,
                port=settings.REDIS_PORT,
                password=settings.REDIS_PASSWORD,
                db=settings.REDIS_DB,
                decode_responses=True,
                socket_timeout=2.0,
                socket_connect_timeout=2.0,
            )
            self._client.ping()
            self._is_available = True
            logger.info(f"Redis Cache Service initialized cleanly at {settings.REDIS_HOST}:{settings.REDIS_PORT}")
        except Exception as err:
            self._is_available = False
            self._client = None
            logger.warning(f"Redis Cache unavailable ({err}). Falling back to direct database/compute execution.")

    @property
    def is_available(self) -> bool:
        if not self._is_available or self._client is None:
            self._connect()
        return self._is_available

    def get_json(self, key: str) -> Optional[Any]:
        """Retrieves and deserializes JSON data from Redis cache."""
        if not self.is_available or self._client is None:
            return None
        try:
            val = self._client.get(key)
            if val:
                return json.loads(val)
        except Exception as err:
            logger.warning(f"Failed to fetch key '{key}' from Redis: {err}")
        return None

    def set_json(self, key: str, value: Any, ttl: Optional[int] = None) -> bool:
        """Serializes and stores data in Redis with a specified TTL (default 6 hours = 21600s)."""
        if not self.is_available or self._client is None:
            return False
        try:
            ttl_seconds = ttl if ttl is not None else settings.REDIS_DEFAULT_TTL
            serialized = json.dumps(value, default=_json_serializer)
            self._client.setex(name=key, time=ttl_seconds, value=serialized)
            return True
        except Exception as err:
            logger.warning(f"Failed to set key '{key}' in Redis: {err}")
            return False

    def delete_key(self, key: str) -> bool:
        """Deletes a single key from Redis."""
        if not self.is_available or self._client is None:
            return False
        try:
            self._client.delete(key)
            return True
        except Exception as err:
            logger.warning(f"Failed to delete key '{key}' from Redis: {err}")
            return False

    def delete_pattern(self, pattern: str) -> int:
        """Deletes all keys matching a pattern (e.g. 'shopper:*', 'analytics:*')."""
        if not self.is_available or self._client is None:
            return 0
        try:
            keys = self._client.keys(pattern)
            if keys:
                return self._client.delete(*keys)
        except Exception as err:
            logger.warning(f"Failed to delete pattern '{pattern}' from Redis: {err}")
        return 0

    def mget_json(self, keys: List[str]) -> Dict[str, Any]:
        """Performs multi-key GET in Redis and returns a dictionary of key-value pairs for hit keys."""
        if not self.is_available or self._client is None or not keys:
            return {}
        try:
            raw_values = self._client.mget(keys)
            result = {}
            for key, raw in zip(keys, raw_values):
                if raw:
                    result[key] = json.loads(raw)
            return result
        except Exception as err:
            logger.warning(f"Failed to mget keys from Redis: {err}")
            return {}

    def mset_json(self, key_values: Dict[str, Any], ttl: Optional[int] = None) -> bool:
        """Stores multiple key-value pairs in Redis using pipeline with TTL (6 hours)."""
        if not self.is_available or self._client is None or not key_values:
            return False
        try:
            ttl_seconds = ttl if ttl is not None else settings.REDIS_DEFAULT_TTL
            pipe = self._client.pipeline()
            for key, val in key_values.items():
                pipe.setex(name=key, time=ttl_seconds, value=json.dumps(val, default=_json_serializer))
            pipe.execute()
            return True
        except Exception as err:
            logger.warning(f"Failed to mset keys in Redis: {err}")
            return False


cache_manager = RedisCacheManager()

