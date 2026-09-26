"""
Analytics service — executive KPIs, demographic breakdowns, and statistical significance tests.
"""
from typing import Dict, Any, List
from fastapi import HTTPException

from core.db.repository import BlackFridayRepository
from ml.models.stats_engine import (
    welch_from_stats, anova_from_stats,
    DIMENSION_TEST_MAP, WELCH_GROUP_KEYS
)
from core.logging import get_logger

logger = get_logger(__name__)

SUPPORTED_DIMENSIONS = {"gender", "age", "marital_status", "occupation", "city_category"}


class AnalyticsService:
    """Service handling executive aggregations and hypothesis testing."""

    @staticmethod
    def get_summary(repo: BlackFridayRepository) -> Dict[str, Any]:
        summary = repo.get_eda_summary()
        if not summary or summary.get("total_orders", 0) == 0:
            raise HTTPException(
                status_code=404,
                detail="Analytics data is not populated. Please ensure database is seeded."
            )
        return {
            "total_orders": summary["total_orders"],
            "total_users": summary["total_users"],
            "total_products": summary["total_products"],
            "avg_order_value": float(summary["avg_order_value"]),
            "total_revenue": float(summary["total_revenue"]),
        }

    @staticmethod
    def get_demographics(dimension: str, repo: BlackFridayRepository) -> List[Dict[str, Any]]:
        if dimension not in SUPPORTED_DIMENSIONS:
            raise HTTPException(status_code=400, detail=f"Unsupported dimension: {dimension}")
        return repo.get_demographic_distribution(dimension)

    @staticmethod
    def get_eda_with_stats(dimension: str, repo: BlackFridayRepository) -> Dict[str, Any]:
        if dimension not in SUPPORTED_DIMENSIONS:
            raise HTTPException(
                status_code=400,
                detail=f"Unsupported dimension '{dimension}'. Supported: {sorted(SUPPORTED_DIMENSIONS)}"
            )

        categories = repo.get_dimension_distribution(dimension)
        group_stats = repo.get_dimension_stats(dimension)

        test_type = DIMENSION_TEST_MAP.get(dimension, "anova")
        if test_type == "welch":
            group_a, group_b = WELCH_GROUP_KEYS[dimension]
            test_res = welch_from_stats(group_stats, group_a, group_b, dimension=dimension)
        else:
            test_res = anova_from_stats(group_stats, dimension=dimension)

        return {
            "dimension": dimension,
            "categories": categories,
            "test_name": test_res["test_name"],
            "test_statistic": test_res["test_statistic"],
            "p_value": test_res["p_value"],
            "is_significant": test_res["is_significant"],
            "interpretation": test_res["interpretation"],
            "details": test_res["details"],
        }


analytics_service = AnalyticsService()
