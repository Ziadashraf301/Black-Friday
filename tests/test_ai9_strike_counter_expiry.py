"""AI-9: a strike counter always gets an expiry, even if an earlier INCR never got its EXPIRE."""

import uuid

import pytest

from ai.guardrails.strike_tracker import StrikeTracker
from core.cache.redis_client import cache_manager


@pytest.fixture
def redis_client():
    if not cache_manager.is_available or cache_manager.client is None:
        pytest.skip("Redis unavailable")
    return cache_manager.client


def test_counter_without_ttl_gets_one_on_the_next_strike(redis_client):
    tracker = StrikeTracker(cache=cache_manager)
    user = f"ai9-{uuid.uuid4().hex}"
    strike_key = f"{tracker.STRIKE_PREFIX}:{user}"
    try:
        redis_client.set(strike_key, 1)  # what a crash between INCR and EXPIRE leaves behind
        assert redis_client.ttl(strike_key) == -1

        assert tracker.record_strike(user) == 2
        assert 0 < redis_client.ttl(strike_key) <= tracker.BAN_TTL_SECONDS
    finally:
        tracker.reset_strikes(user)


def test_ttl_is_not_extended_by_later_strikes(redis_client):
    tracker = StrikeTracker(cache=cache_manager)
    user = f"ai9-{uuid.uuid4().hex}"
    strike_key = f"{tracker.STRIKE_PREFIX}:{user}"
    try:
        tracker.record_strike(user, ban_ttl=100)
        redis_client.expire(strike_key, 50)
        tracker.record_strike(user, ban_ttl=100)
        assert redis_client.ttl(strike_key) <= 50
    finally:
        tracker.reset_strikes(user)


def test_three_strikes_ban(redis_client):
    tracker = StrikeTracker(cache=cache_manager)
    user = f"ai9-{uuid.uuid4().hex}"
    try:
        assert [tracker.record_strike(user) for _ in range(3)] == [1, 2, 3]
        assert tracker.is_banned(user)
    finally:
        tracker.reset_strikes(user)
