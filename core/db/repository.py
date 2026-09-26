"""
Unified BlackFridayRepository composing domain repositories.
"""
from core.db.repositories.base import BaseRepository
from core.db.repositories.warehouse_repo import WarehouseRepository
from core.db.repositories.analytics_repo import AnalyticsRepository
from core.db.repositories.segmentation_repo import SegmentationRepository
from core.db.repositories.recommendation_repo import RecommendationRepository
from core.db.repositories.user_repo import UserRepository


class BlackFridayRepository(
    WarehouseRepository,
    AnalyticsRepository,
    SegmentationRepository,
    RecommendationRepository,
    UserRepository
):
    """
    Unified Data Warehouse & Application Repository.
    Composes focused domain repositories:
      - WarehouseRepository (raw & cleaned transaction data, bulk loading)
      - AnalyticsRepository (EDA summary & demographic distributions)
      - SegmentationRepository (customer personas & cluster metrics)
      - RecommendationRepository (PageRank, HITS, bundle rules, categories)
      - UserRepository (user registration, authentication & purchase history)
    """

    def __init__(self, engine=None):
        super().__init__(engine=engine)
