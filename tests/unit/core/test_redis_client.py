"""CORE-1: the Redis JSON serializer must terminate on arbitrary objects (e.g. MagicMock) and unwrap numpy types.

CORE-3: after the cooldown expires only one thread may attempt the reconnect."""

import concurrent.futures
import datetime
import decimal
import json
import time
from unittest.mock import MagicMock

import numpy as np

from core.cache.redis_client import RedisCacheManager, _json_serializer


def _dumps(value):
    return json.dumps(value, default=_json_serializer)


def test_numpy_scalars_and_arrays_are_unwrapped():
    payload = {"i": np.int64(3), "f": np.float32(1.5), "b": np.bool_(True), "a": np.array([1, 2])}
    assert json.loads(_dumps(payload)) == {"i": 3, "f": 1.5, "b": True, "a": [1, 2]}


def test_decimal_and_dates_are_converted():
    out = json.loads(_dumps({"d": decimal.Decimal("1.5"), "t": datetime.date(2026, 1, 2)}))
    assert out == {"d": 1.5, "t": "2026-01-02"}


def test_object_with_item_attribute_does_not_hang():
    out = json.loads(_dumps({"m": MagicMock()}))
    assert isinstance(out["m"], str)


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
