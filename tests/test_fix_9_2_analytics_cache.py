"""
Regression tests for Fix 9.2 (and WP6 decoupling):
- AnalyticsRepository is pure data access: passes with Redis stopped and makes zero cache calls.
- AnalyticsService provides caching with 1-hour TTL via cached_json decorator.
- AnalyticsService falls back gracefully to DB execution when Redis is down.
- AnalyticsService cache invalidation after pipeline reloads.
"""
from unittest.mock import patch, MagicMock, PropertyMock
from core.db.repositories.analytics_repo import AnalyticsRepository
from apps.api.services.analytics_service import AnalyticsService, analytics_service
from core.cache import cache_manager, RedisCacheManager


def test_repository_pure_data_access_with_redis_stopped():
    """Verify AnalyticsRepository operates as pure data access without invoking Redis."""
    repo = AnalyticsRepository()
    with patch.object(cache_manager, "get_json") as mock_get_json, \
         patch.object(cache_manager, "set_json") as mock_set_json, \
         patch.object(RedisCacheManager, "is_available", new_callable=PropertyMock, return_value=False):
        res = repo.get_eda_summary()
        assert isinstance(res, dict)
        assert "total_orders" in res
        mock_get_json.assert_not_called()
        mock_set_json.assert_not_called()


def test_service_caching_and_ttl():
    """Verify AnalyticsService caches EDA summary in Redis with 1-hour (3600s) TTL."""
    cache_key = "analytics:eda_summary"
    cache_manager.delete_key(cache_key)

    repo = AnalyticsRepository()
    # 1. First call: executes against repository and populates Redis cache
    res1 = analytics_service.get_summary(repo)
    assert isinstance(res1, dict)
    assert "total_orders" in res1

    # Verify key was written to Redis
    cached = cache_manager.get_json(cache_key)
    assert cached is not None
    assert cached["total_orders"] == res1["total_orders"]

    # Verify TTL is near 3600s
    if cache_manager.client is not None:
        ttl = cache_manager.client.ttl(cache_key)
        assert 0 < ttl <= 3600

    # 2. Second call: served from Redis cache without querying repository
    mock_repo = MagicMock(spec=repo)
    res2 = analytics_service.get_summary(mock_repo)
    mock_repo.get_eda_summary.assert_not_called()
    assert res2["total_orders"] == res1["total_orders"]


def test_service_fallback_when_redis_is_down():
    """Verify AnalyticsService executes successfully against DB when Redis is unavailable."""
    cache_key = "analytics:eda_summary"
    cache_manager.delete_key(cache_key)

    repo = AnalyticsRepository()
    with patch.object(RedisCacheManager, "is_available", new_callable=PropertyMock, return_value=False), \
         patch.object(cache_manager, "get_json", return_value=None), \
         patch.object(cache_manager, "set_json", return_value=False):
        res = analytics_service.get_summary(repo)
        assert isinstance(res, dict)
        assert "total_orders" in res
        assert res["total_orders"] > 0


def test_service_invalidation_after_reload():
    """Verify analytics cache entries are invalidated after reload pipeline."""
    cache_key = "analytics:eda_summary"
    repo = AnalyticsRepository()

    # Populate cache
    _ = analytics_service.get_summary(repo)
    assert cache_manager.get_json(cache_key) is not None

    # Simulate reload pipeline invalidation (e.g., ingest, seed_curated, preprocess)
    deleted = cache_manager.delete_pattern("analytics:*")
    assert deleted >= 1
    assert cache_manager.get_json(cache_key) is None

    # Subsequent call re-populates cache
    _ = analytics_service.get_summary(repo)
    assert cache_manager.get_json(cache_key) is not None
