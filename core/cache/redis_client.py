import json
import decimal
import datetime
import threading
import time
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

    def __init__(self, db: Optional[int] = None):
        self._client: Optional[redis.Redis] = None
        self._is_available = False
        self._db = db
        self._last_failure_time: float = 0.0
        self._cooldown_seconds: float = 15.0
        self._lock = threading.Lock()
        self._connect()

    def _connect(self):
        with self._lock:
            try:
                target_db = self._db if self._db is not None else settings.REDIS_DB
                self._client = redis.Redis(
                    host=settings.REDIS_HOST,
                    port=settings.REDIS_PORT,
                    password=settings.REDIS_PASSWORD,
                    db=target_db,
                    decode_responses=True,
                    socket_timeout=2.0,
                    socket_connect_timeout=2.0,
                )
                self._client.ping()
                self._is_available = True
                self._last_failure_time = 0.0
                logger.info(f"Redis Cache Service initialized cleanly at {settings.REDIS_HOST}:{settings.REDIS_PORT} (DB {target_db})")
            except Exception as err:
                self._is_available = False
                self._client = None
                self._last_failure_time = time.monotonic()
                logger.warning(f"Redis Cache unavailable ({err}). Falling back to direct database/compute execution.")

    def _handle_disconnect(self, err: Exception):
        with self._lock:
            self._is_available = False
            self._client = None
            self._last_failure_time = time.monotonic()
            logger.warning(f"Redis Cache connection lost ({err}). Entering 15s cooldown.")

    def reconnect(self, db: Optional[int] = None):
        """Forces a reconnection attempt, optionally targeting a different database index."""
        with self._lock:
            if db is not None:
                self._db = db
            self._last_failure_time = 0.0
        self._connect()

    @property
    def is_available(self) -> bool:
        if self._is_available and self._client is not None:
            return True
        now = time.monotonic()
        if now - self._last_failure_time < self._cooldown_seconds:
            return False
        self._connect()
        return self._is_available

    @property
    def client(self) -> Optional[redis.Redis]:
        """Public thread-safe accessor for the underlying Redis client."""
        if self.is_available:
            return self._client
        return None

    def get(self, key: str) -> Optional[str]:
        """Retrieves raw string data from Redis cache."""
        if not self.is_available or self._client is None:
            return None
        try:
            return self._client.get(key)
        except (redis.ConnectionError, redis.TimeoutError) as err:
            self._handle_disconnect(err)
            return None
        except Exception as err:
            logger.warning(f"Failed to fetch key '{key}' from Redis: {err}")
            return None

    def set(self, key: str, value: str, ttl: Optional[int] = None) -> bool:
        """Stores raw string data in Redis with a specified TTL."""
        if not self.is_available or self._client is None:
            return False
        try:
            ttl_seconds = ttl if ttl is not None else settings.REDIS_DEFAULT_TTL
            self._client.setex(name=key, time=ttl_seconds, value=value)
            return True
        except (redis.ConnectionError, redis.TimeoutError) as err:
            self._handle_disconnect(err)
            return False
        except Exception as err:
            logger.warning(f"Failed to set key '{key}' in Redis: {err}")
            return False

    def get_json(self, key: str) -> Optional[Any]:
        """Retrieves and deserializes JSON data from Redis cache."""
        if not self.is_available or self._client is None:
            return None
        try:
            val = self._client.get(key)
            if val:
                return json.loads(val)
        except (redis.ConnectionError, redis.TimeoutError) as err:
            self._handle_disconnect(err)
            return None
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
        except (redis.ConnectionError, redis.TimeoutError) as err:
            self._handle_disconnect(err)
            return False
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
        except (redis.ConnectionError, redis.TimeoutError) as err:
            self._handle_disconnect(err)
            return False
        except Exception as err:
            logger.warning(f"Failed to delete key '{key}' from Redis: {err}")
            return False

    def delete_pattern(self, pattern: str, batch_size: int = 500) -> int:
        """Deletes all keys matching a pattern using non-blocking scan_iter batches."""
        if not self.is_available or self._client is None:
            return 0
        try:
            deleted_count = 0
            batch = []
            for key in self._client.scan_iter(match=pattern, count=batch_size):
                batch.append(key)
                if len(batch) >= batch_size:
                    deleted_count += self._client.delete(*batch)
                    batch = []
            if batch:
                deleted_count += self._client.delete(*batch)
            return deleted_count
        except (redis.ConnectionError, redis.TimeoutError) as err:
            self._handle_disconnect(err)
            return 0
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
        except (redis.ConnectionError, redis.TimeoutError) as err:
            self._handle_disconnect(err)
            return {}
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
        except (redis.ConnectionError, redis.TimeoutError) as err:
            self._handle_disconnect(err)
            return False
        except Exception as err:
            logger.warning(f"Failed to mset keys in Redis: {err}")
            return False


cache_manager = RedisCacheManager()

