"""
Domain repositories package.
"""
from core.db.repositories.base import BaseRepository
from core.db.repositories.warehouse_repo import WarehouseRepository
from core.db.repositories.analytics_repo import AnalyticsRepository
from core.db.repositories.segmentation_repo import SegmentationRepository
from core.db.repositories.recommendation_repo import RecommendationRepository
from core.db.repositories.user_repo import UserRepository

__all__ = [
    "BaseRepository",
    "WarehouseRepository",
    "AnalyticsRepository",
    "SegmentationRepository",
    "RecommendationRepository",
    "UserRepository",
]
