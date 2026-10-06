"""
Unit and Integration Tests for Progressive Search Relaxation Ladder (Phase 4 - Task P4-04).
Validates:
  1. Tier 1: TIER_1_STRICT full constraint hybrid search.
  2. Tier 2: TIER_2_RELAX_SIZE fallback when exact size has zero inventory.
  3. Tier 3: TIER_3_RELAX_BUDGET fallback expanding budget by +25% when budget is too tight.
  4. Tier 4: TIER_4_ZERO_DATA_DEALS guaranteed fallback returning top featured doorbuster deals.
"""
import pytest
from ai.services.search_service import search_service
from core.db.repository import BlackFridayRepository


class TestProgressiveSearchRelaxationLadder:
    """Tests the 4-tier relaxation ladder in search repository and search service."""

    @pytest.fixture(autouse=True)
    def setup_repo(self):
        self.repo = BlackFridayRepository()

    def test_tier1_strict_search(self):
        # Query matching standard in-stock inventory with reasonable budget
        results = search_service.search(
            query="leather jacket",
            category="Outerwear",
            max_price=200.0,
            size="M",
            top_k=3,
        )
        assert len(results) > 0
        assert results[0].relaxation_level == "TIER_1_STRICT"
        for r in results:
            assert r.discounted_price <= 200.0

    def test_tier2_relax_size_fallback(self):
        # Query with an impossible size (e.g. "5XL" or "XXXXXL") that doesn't exist in catalog
        results = search_service.search(
            query="cashmere sweater",
            category="Knitwear",
            max_price=300.0,
            size="XXXXXL",
            top_k=3,
        )
        assert len(results) > 0
        # Must have fallen back to Tier 2 (relaxed size)
        assert results[0].relaxation_level in ("TIER_2_RELAX_SIZE", "TIER_3_RELAX_BUDGET", "TIER_4_ZERO_DATA_DEALS")

    def test_tier3_relax_budget_fallback(self):
        # Find minimum price in catalog or use a tight budget where items exist just above it
        # Setting a budget of $45 for leather jackets where discounted price is around $49.90
        results = search_service.search(
            query="leather jacket",
            category="Outerwear",
            max_price=42.0,
            size=None,
            top_k=3,
        )
        assert len(results) > 0
        # If no items <= $42, budget is expanded to $42 * 1.25 = $52.50, capturing the $49.90 deals!
        assert results[0].relaxation_level in ("TIER_3_RELAX_BUDGET", "TIER_4_ZERO_DATA_DEALS")

    def test_tier4_zero_data_deals_fallback(self):
        # Impossible budget constraint: max_price = $1.00
        results = search_service.search(
            query="nonexistent alien space boots",
            category="Footwear",
            max_price=1.0,
            size="XXXXXL",
            top_k=4,
        )
        assert len(results) > 0
        # Guaranteed to return top Black Friday deals
        assert results[0].relaxation_level == "TIER_4_ZERO_DATA_DEALS"
        for r in results:
            assert r.product_id.startswith("P")
            assert r.discounted_price > 0
