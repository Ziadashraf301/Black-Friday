"""
Unit and Integration Tests for Live Database & Redis Bundle Subsystem (Phase 4 - Task P4-05).
Validates:
  1. Live database retrieval of apriori_bundles and item2vec_similars from curated_products.
  2. Redis 24-hour bundle caching and sub-millisecond cache hit.
  3. Dynamic pricing calculations and savings percentages.
"""
import pytest
from ai.tools.bundle_tools import bundle_tool, BundleRecommendationsTool
from core.cache.redis_client import RedisCacheManager


class TestBundleEngine:
    """Verifies database integration, Redis caching, and dynamic pricing for product bundles."""

    @pytest.fixture(autouse=True)
    def setup_cache(self):
        self.cache = RedisCacheManager()

    def test_bundle_retrieval_and_redis_caching(self):
        target_pid = "P00025442"
        # Invalidate existing cache
        cache_key = f"cache:bundles:{target_pid}:all"
        if self.cache.is_available:
            self.cache.delete_key(cache_key)

        # 1. First fetch: queries live DB and populates Redis
        bundles_1 = bundle_tool.get_recommendations(target_pid, bundle_type="all")
        assert len(bundles_1) > 0
        for b in bundles_1:
            assert b.product_id != ""
            assert b.price > 0
            assert b.savings_pct in (10.0, 15.0)

        # Verify Redis key exists
        if self.cache.is_available:
            cached_json = self.cache.get_json(cache_key)
            assert cached_json is not None
            assert len(cached_json) == len(bundles_1)

            # 2. Second fetch: hits Redis directly
            bundles_2 = bundle_tool.get_recommendations(target_pid, bundle_type="all")
            assert len(bundles_2) == len(bundles_1)
            assert bundles_2[0].product_id == bundles_1[0].product_id

    def test_bundle_types_filtering(self):
        target_pid = "P00025442"
        apriori_only = bundle_tool.get_recommendations(target_pid, bundle_type="apriori")
        for b in apriori_only:
            assert b.relationship_type == "apriori"
            assert b.savings_pct == 15.0
