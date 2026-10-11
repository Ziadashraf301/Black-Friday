"""
Caching decorators for application services.
Provides non-blocking, fault-tolerant Redis JSON caching with transparent fallback to execution.
"""
import functools
import inspect
from typing import Callable, Any, Optional, Union
from core.cache.redis_client import cache_manager
from core.logging import get_logger

logger = get_logger(__name__)


def _resolve_cache_key(
    key_spec: Union[str, Callable[..., str]],
    fn: Callable[..., Any],
    args: tuple,
    kwargs: dict,
) -> Optional[str]:
    """Resolves dynamic cache key from static string, format pattern, or callable.

    Returns None when a dynamic key cannot be built, so the caller bypasses the cache instead of
    letting every call share one unformatted key.
    """
    try:
        if callable(key_spec):
            return key_spec(*args, **kwargs)

        if "{" in key_spec and "}" in key_spec:
            bound = inspect.signature(fn).bind_partial(*args, **kwargs)
            bound.apply_defaults()
            return key_spec.format(**bound.arguments)
    except Exception as exc:
        logger.warning(f"Cache key resolution failed for '{getattr(fn, '__name__', fn)}': {exc}. Bypassing cache.")
        return None

    return str(key_spec)


def cached_json(key: Union[str, Callable[..., str]], ttl: int = 3600):
    """
    Decorator for caching JSON-serializable service function returns in Redis.
    Supports both synchronous and asynchronous functions.

    Fault tolerance:
    - If Redis is unavailable or fails, invokes the wrapped function directly.
    - Zero exceptions bubbled up from cache failures.
    - Zero delay / hangs when Redis is disconnected.
    """
    def decorator(fn: Callable[..., Any]) -> Callable[..., Any]:
        if inspect.iscoroutinefunction(fn):
            @functools.wraps(fn)
            async def async_wrapper(*args, **kwargs):
                cache_key = _resolve_cache_key(key, fn, args, kwargs)
                if cache_key is None:
                    return await fn(*args, **kwargs)
                # 1. Try cache read
                try:
                    if cache_manager.is_available:
                        cached = cache_manager.get_json(cache_key)
                        if cached is not None:
                            return cached
                except Exception as exc:
                    logger.warning(f"Cache get failed for '{cache_key}': {exc}")

                # 2. Execute underlying function
                result = await fn(*args, **kwargs)

                # 3. Try cache write
                try:
                    if result is not None and cache_manager.is_available:
                        cache_manager.set_json(cache_key, result, ttl=ttl)
                except Exception as exc:
                    logger.warning(f"Cache set failed for '{cache_key}': {exc}")

                return result

            return async_wrapper
        else:
            @functools.wraps(fn)
            def sync_wrapper(*args, **kwargs):
                cache_key = _resolve_cache_key(key, fn, args, kwargs)
                if cache_key is None:
                    return fn(*args, **kwargs)
                # 1. Try cache read
                try:
                    if cache_manager.is_available:
                        cached = cache_manager.get_json(cache_key)
                        if cached is not None:
                            return cached
                except Exception as exc:
                    logger.warning(f"Cache get failed for '{cache_key}': {exc}")

                # 2. Execute underlying function
                result = fn(*args, **kwargs)

                # 3. Try cache write
                try:
                    if result is not None and cache_manager.is_available:
                        cache_manager.set_json(cache_key, result, ttl=ttl)
                except Exception as exc:
                    logger.warning(f"Cache set failed for '{cache_key}': {exc}")

                return result

            return sync_wrapper

    return decorator
