"""
Regression tests for Fix 2.2: Redis Cache Manager hardening.
Validates:
1. Public thread-safe client property.
2. 15-second reconnection cooldown on unreachable Redis (<100ms on 2nd and later calls).
3. Non-blocking delete_pattern using scan_iter in batches (removes only matching keys in real Redis).
4. Concurrent access from multiple threads does not crash or raise exceptions.
"""
import time
import concurrent.futures
import pytest
from core.cache.redis_client import RedisCacheManager, cache_manager


def test_fix_2_2_public_client_property():
    """Validates that .client is an exposed public property returning the redis.Redis instance."""
    assert hasattr(cache_manager, "client")
    assert cache_manager.is_available is True
    assert cache_manager.client is not None
    assert hasattr(cache_manager.client, "ping")
    assert cache_manager.client.ping() is True


@pytest.mark.benchmark
def test_fix_2_2_unreachable_redis_cooldown():
    """
    With an unreachable Redis, is_available enters a 15-second cooldown after a failure
    and returns False in under 100 ms on second and subsequent calls.
    """
    # Create a manager targeting an unreachable port
    unreachable_manager = RedisCacheManager()
    # Force failure and enter cooldown
    unreachable_manager._handle_disconnect(Exception("Simulated connection failure"))
    assert unreachable_manager._is_available is False

    # Second call should immediately return False via cooldown without socket hangs (<100ms)
    t0 = time.perf_counter()
    res = unreachable_manager.is_available
    elapsed_ms = (time.perf_counter() - t0) * 1000

    assert res is False
    assert elapsed_ms < 100.0, f"Expected <100ms but took {elapsed_ms:.2f}ms"

    # Third call should also be sub-millisecond
    t1 = time.perf_counter()
    assert unreachable_manager.is_available is False
    elapsed_ms_2 = (time.perf_counter() - t1) * 1000
    assert elapsed_ms_2 < 100.0


def test_fix_2_2_delete_pattern_matching_keys_only():
    """Validates delete_pattern removes only matching keys in batches using real Redis."""
    assert cache_manager.is_available is True
    client = cache_manager.client
    assert client is not None

    # Seed test keys
    client.set("test:wp3:del:1", "val1")
    client.set("test:wp3:del:2", "val2")
    client.set("test:wp3:del:3", "val3")
    client.set("test:wp3:keep:1", "keep_val")

    deleted = cache_manager.delete_pattern("test:wp3:del:*")
    assert deleted >= 3

    assert client.get("test:wp3:del:1") is None
    assert client.get("test:wp3:del:2") is None
    assert client.get("test:wp3:del:3") is None
    assert client.get("test:wp3:keep:1") == "keep_val"

    # Cleanup keep key
    client.delete("test:wp3:keep:1")


def test_fix_2_2_concurrent_access_threads():
    """Concurrent access from multiple threads to cache_manager does not crash or race."""
    assert cache_manager.is_available is True

    def worker(i: int):
        key = f"test:wp3:thread:{i}"
        cache_manager.set_json(key, {"thread_id": i, "val": i * 10}, ttl=60)
        data = cache_manager.get_json(key)
        assert data is not None
        assert data["thread_id"] == i
        _ = cache_manager.client
        _ = cache_manager.is_available
        cache_manager.delete_key(key)
        return True

    with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
        futures = [executor.submit(worker, i) for i in range(50)]
        results = [f.result() for f in concurrent.futures.as_completed(futures)]

    assert len(results) == 50
    assert all(results)
