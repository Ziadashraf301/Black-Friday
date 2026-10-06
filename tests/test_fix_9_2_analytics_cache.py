"""
Regression test for Fix 9.2:
- Distributed Redis caching in AnalyticsRepository
- Shared cache across separate repository instances
- Graceful database execution fallback when Redis is unavailable
"""
from unittest.mock import patch
from core.db.repositories.analytics_repo import AnalyticsRepository
from core.cache import cache_manager


def test_two_repository_instances_share_cached_summary():
    """Verify distinct repository instances share the Redis-cached EDA summary."""
    cache_key = "analytics:eda_summary"
    cache_manager.delete_key(cache_key)

    repo1 = AnalyticsRepository()
    result1 = repo1.get_eda_summary()
    assert isinstance(result1, dict)
    assert "total_orders" in result1

    # Verify key was written to Redis
    cached = cache_manager.get_json(cache_key)
    assert cached is not None
    assert cached["total_orders"] == result1["total_orders"]

    # repo2 is a completely new instance
    repo2 = AnalyticsRepository()
    # When conn.execute would be called, we ensure repo2 served from Redis cache
    with patch.object(repo2.engine, "connect") as mock_connect:
        result2 = repo2.get_eda_summary()
        # Should not connect to DB because cache hit
        mock_connect.assert_not_called()
        assert result2["total_orders"] == result1["total_orders"]


def test_eda_summary_works_when_redis_is_down():
    """Verify get_eda_summary executes successfully against DB when Redis is unavailable."""
    repo = AnalyticsRepository()
    with patch.object(cache_manager, "get_json", return_value=None), \
         patch.object(cache_manager, "set_json", return_value=False):
        result = repo.get_eda_summary()
        assert isinstance(result, dict)
        assert "total_orders" in result
        assert result["total_orders"] > 0
