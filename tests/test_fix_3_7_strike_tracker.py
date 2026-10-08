"""
Regression tests for Fix 3.7: StrikeTracker Redis operations and fallback hardening.
Validates:
1. StrikeTracker consumes public API (cache_manager.client, cache_manager.get, cache_manager.delete_key).
2. Strikes increment in REAL Redis under 'security:strikes:<id>'.
3. Lockout ban writes to REAL Redis under 'security:banned:<id>' with TTL.
4. Expiry works with short TTL override (ban expires in Redis).
5. In-memory fallback works seamlessly when Redis is completely unavailable.
"""
import time
import pytest
from ai.guardrails.strike_tracker import StrikeTracker, strike_tracker
from core.cache.redis_client import cache_manager, RedisCacheManager


@pytest.fixture
def clean_test_user():
    user_id = "test_wp3_security_user_77"
    strike_tracker.reset_strikes(user_id)
    yield user_id
    strike_tracker.reset_strikes(user_id)


def test_fix_3_7_real_redis_strike_and_ban(clean_test_user):
    """Verifies strikes and bans write to REAL Redis via public API."""
    user = clean_test_user
    assert cache_manager.is_available is True
    client = cache_manager.client
    assert client is not None

    strike_key = f"security:strikes:{user}"
    ban_key = f"security:banned:{user}"

    # Verify initial state
    assert strike_tracker.is_banned(user) is False
    assert client.get(strike_key) is None
    assert client.get(ban_key) is None

    # Strike 1
    s1 = strike_tracker.record_strike(user)
    assert s1 == 1
    assert client.get(strike_key) == "1"
    assert client.ttl(strike_key) > 0
    assert strike_tracker.is_banned(user) is False

    # Strike 2
    s2 = strike_tracker.record_strike(user)
    assert s2 == 2
    assert client.get(strike_key) == "2"
    assert strike_tracker.is_banned(user) is False

    # Strike 3 -> Ban triggered in Redis
    s3 = strike_tracker.record_strike(user)
    assert s3 == 3
    assert client.get(strike_key) == "3"
    assert client.get(ban_key) == "BANNED_FOR_24H"
    assert client.ttl(ban_key) > 0
    assert strike_tracker.is_banned(user) is True


def test_fix_3_7_real_redis_ban_expiry(clean_test_user):
    """Verifies that short TTL override on ban expires properly in Redis."""
    user = clean_test_user
    client = cache_manager.client
    assert client is not None

    # Trigger lockout with 1-second TTL override
    strike_tracker.record_strike(user, ban_ttl=1)
    strike_tracker.record_strike(user, ban_ttl=1)
    strike_tracker.record_strike(user, ban_ttl=1)

    assert strike_tracker.is_banned(user) is True
    assert client.get(f"security:banned:{user}") is not None

    # Wait for expiry
    time.sleep(1.2)

    assert client.get(f"security:banned:{user}") is None
    assert strike_tracker.is_banned(user) is False


def test_fix_3_7_in_memory_fallback_when_redis_down():
    """Verifies strike tracking and lockout work via in-memory fallback when Redis is offline."""
    # Create an offline cache manager
    offline_cache = RedisCacheManager()
    offline_cache._handle_disconnect(Exception("Offline simulated"))
    assert offline_cache.is_available is False

    offline_tracker = StrikeTracker(cache=offline_cache)
    user = "offline_attacker_42"

    assert offline_tracker.is_banned(user) is False

    s1 = offline_tracker.record_strike(user, ban_ttl=1)
    assert s1 == 1
    assert offline_tracker.is_banned(user) is False

    s2 = offline_tracker.record_strike(user, ban_ttl=1)
    assert s2 == 2
    assert offline_tracker.is_banned(user) is False

    s3 = offline_tracker.record_strike(user, ban_ttl=1)
    assert s3 == 3
    assert offline_tracker.is_banned(user) is True

    # Test expiry in memory
    time.sleep(1.2)
    assert offline_tracker.is_banned(user) is False
