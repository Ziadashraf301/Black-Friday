"""CORE-3: after the cooldown expires only one thread may attempt the reconnect."""
import concurrent.futures
import time

from core.cache.redis_client import RedisCacheManager


def test_single_reconnect_attempt_after_cooldown(monkeypatch):
    manager = RedisCacheManager.__new__(RedisCacheManager)
    import threading

    manager._client = None
    manager._is_available = False
    manager._db = None
    manager._last_failure_time = time.monotonic() - 100.0
    manager._cooldown_seconds = 15.0
    manager._lock = threading.RLock()

    attempts = []

    def fake_connect():
        with manager._lock:
            attempts.append(1)
            time.sleep(0.05)
            manager._is_available = False
            manager._last_failure_time = time.monotonic()

    monkeypatch.setattr(manager, "_connect", fake_connect)

    with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
        results = list(pool.map(lambda _: manager.is_available, range(8)))

    assert results == [False] * 8
    assert len(attempts) == 1
