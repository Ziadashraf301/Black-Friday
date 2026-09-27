"""
Analytics service — executive KPIs and demographic breakdowns.
"""
from typing import Dict, Any, List
from fastapi import HTTPException

from core.db.repository import BlackFridayRepository
from core.logging import get_logger

logger = get_logger(__name__)

SUPPORTED_DIMENSIONS = {"gender", "age", "marital_status", "occupation", "city_category"}


class AnalyticsService:
    """Service handling executive aggregations."""

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


analytics_service = AnalyticsService()
