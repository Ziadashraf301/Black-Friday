"""
Regression tests for Fix 8.3: BundleRecommendationsTool cache singleton reuse.
Validates:
1. BundleRecommendationsTool reuses singleton cache_manager by default.
2. No unnecessary unpooled RedisCacheManager() instances are created.
"""
from ai.tools.bundle_tools import BundleRecommendationsTool, bundle_tool
from core.cache.redis_client import cache_manager


def test_fix_8_3_bundle_recommendations_tool_reuses_singleton_cache():
    """Validates that BundleRecommendationsTool defaults to singleton cache_manager."""
    tool = BundleRecommendationsTool()
    assert tool._cache is cache_manager
    assert bundle_tool._cache is cache_manager
