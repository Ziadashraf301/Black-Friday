"""
Analytics service — executive KPIs and demographic breakdowns with persistent Redis 6-hour caching.
"""
from typing import Dict, Any, List
from fastapi import HTTPException

from core.db.repository import BlackFridayRepository
from core.cache import cache_manager
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

SUPPORTED_DIMENSIONS = {"gender", "age", "marital_status", "occupation", "city_category"}


class AnalyticsService:
    """Service handling executive aggregations with persistent Redis caching."""

    @staticmethod
    def get_summary(repo: BlackFridayRepository) -> Dict[str, Any]:
        """Returns executive KPI summary. Executed once on DB, cached for 6 hours in Redis."""
        cache_key = "analytics:summary"
        cached = cache_manager.get_json(cache_key)
        if cached:
            return cached

        summary = repo.get_eda_summary()
        if not summary or summary.get("total_orders", 0) == 0:
            raise HTTPException(
                status_code=404,
                detail="Analytics data is not populated. Please ensure database is seeded."
            )
        result = {
            "total_orders": summary["total_orders"],
            "total_users": summary["total_users"],
            "total_products": summary["total_products"],
            "avg_order_value": float(summary["avg_order_value"]),
            "total_revenue": float(summary["total_revenue"]),
        }
        cache_manager.set_json(cache_key, result, ttl=settings.REDIS_DEFAULT_TTL)
        return result

    @staticmethod
    def get_demographics(dimension: str, repo: BlackFridayRepository) -> List[Dict[str, Any]]:
        """Returns demographic distribution with 6-hour Redis caching."""
        if dimension not in SUPPORTED_DIMENSIONS:
            raise HTTPException(status_code=400, detail=f"Unsupported dimension: {dimension}")

        cache_key = f"analytics:demographics:{dimension}"
        cached = cache_manager.get_json(cache_key)
        if cached:
            return cached

        data = repo.get_demographic_distribution(dimension)
        if data:
            cache_manager.set_json(cache_key, data, ttl=settings.REDIS_DEFAULT_TTL)
        return data


analytics_service = AnalyticsService()
