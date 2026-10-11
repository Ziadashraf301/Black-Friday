"""CORE-2: a cache key that cannot be resolved must bypass the cache, not collapse to one shared key."""
import asyncio

from core.cache import decorators
from core.cache.decorators import cached_json


class FakeCache:
    is_available = True

    def __init__(self):
        self.store = {}

    def get_json(self, key):
        return self.store.get(key)

    def set_json(self, key, value, ttl=None):
        self.store[key] = value
        return True


def test_unresolvable_key_bypasses_cache(monkeypatch):
    fake = FakeCache()
    monkeypatch.setattr(decorators, "cache_manager", fake)

    @cached_json("thing:{missing}")
    def compute(x):
        return x * 2

    assert compute(1) == 2
    assert compute(2) == 4
    assert fake.store == {}


def test_failing_key_callable_bypasses_cache(monkeypatch):
    fake = FakeCache()
    monkeypatch.setattr(decorators, "cache_manager", fake)

    @cached_json(lambda x: 1 / 0)
    async def compute(x):
        return x + 1

    assert asyncio.run(compute(1)) == 2
    assert fake.store == {}


def test_formatted_key_still_caches_per_argument(monkeypatch):
    fake = FakeCache()
    monkeypatch.setattr(decorators, "cache_manager", fake)

    @cached_json("thing:{x}")
    def compute(x):
        return x * 2

    assert (compute(1), compute(2)) == (2, 4)
    assert fake.store == {"thing:1": 2, "thing:2": 4}
