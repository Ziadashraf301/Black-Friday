"""
Analytics service — executive KPIs and demographic breakdowns with persistent Redis 6-hour caching.
Decoupled from web framework exceptions using domain exceptions.
"""
from typing import Dict, Any, List, Optional

from core.db.repository import BlackFridayRepository
from core.cache import cache_manager, cached_json
from core.config import settings
from core.exceptions import DataUnavailableError, ValidationError
from core.logging import get_logger

logger = get_logger(__name__)

SUPPORTED_DIMENSIONS = {"gender", "age", "marital_status", "occupation", "city_category"}


class AnalyticsService:
    """Service handling executive aggregations with persistent Redis caching via decorators."""

    @staticmethod
    @cached_json("analytics:eda_summary", ttl=3600)
    def get_summary(repo: Optional[BlackFridayRepository] = None) -> Dict[str, Any]:
        """Returns executive KPI summary. Cached for 1 hour (3600s) in Redis."""
        target_repo = repo or BlackFridayRepository()
        summary = target_repo.get_eda_summary()
        if not summary or summary.get("total_orders", 0) == 0:
            raise DataUnavailableError(
                "Analytics data is not populated. Please ensure database is seeded."
            )
        return {
            "total_orders": summary["total_orders"],
            "total_users": summary["total_users"],
            "total_products": summary["total_products"],
            "avg_order_value": float(summary["avg_order_value"]),
            "total_revenue": float(summary["total_revenue"]),
        }

    get_eda_summary = get_summary

    @staticmethod
    @cached_json("analytics:demographics:{dimension}", ttl=settings.REDIS_DEFAULT_TTL)
    def get_demographics(dimension: str, repo: Optional[BlackFridayRepository] = None) -> List[Dict[str, Any]]:
        """Returns demographic distribution with Redis caching."""
        if dimension not in SUPPORTED_DIMENSIONS:
            raise ValidationError(f"Unsupported dimension: {dimension}")

        target_repo = repo or BlackFridayRepository()
        data = target_repo.get_demographic_distribution(dimension)
        return data or []


analytics_service = AnalyticsService()
