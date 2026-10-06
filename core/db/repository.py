"""
Unified BlackFridayRepository composing domain repositories via composition.
"""
from typing import Any, List
from core.db.repositories.base import BaseRepository
from core.db.repositories.warehouse_repo import WarehouseRepository
from core.db.repositories.analytics_repo import AnalyticsRepository
from core.db.repositories.segmentation_repo import SegmentationRepository
from core.db.repositories.recommendation_repo import RecommendationRepository
from core.db.repositories.user_repo import UserRepository


class BlackFridayRepository(BaseRepository):
    """
    Unified Data Warehouse & Application Repository facade using composition.
    Aggregates domain repositories with full backward-compatible delegation:
      - warehouse: WarehouseRepository (raw & cleaned transaction data, bulk loading)
      - analytics: AnalyticsRepository (EDA summary & demographic distributions)
      - segmentation: SegmentationRepository (customer personas & cluster metrics)
      - recommendation: RecommendationRepository (PageRank, HITS, bundle rules, categories)
      - users: UserRepository (user registration, authentication & purchase history)
    """

    def __init__(self, engine=None):
        super().__init__(engine=engine)
        self.warehouse = WarehouseRepository(engine=self.engine)
        self.analytics = AnalyticsRepository(engine=self.engine)
        self.segmentation = SegmentationRepository(engine=self.engine)
        self.recommendation = RecommendationRepository(engine=self.engine)
        self.users = UserRepository(engine=self.engine)
        self._sub_repos = (
            self.warehouse,
            self.analytics,
            self.segmentation,
            self.recommendation,
            self.users,
        )

    def __getattr__(self, name: str) -> Any:
        for sub_repo in self._sub_repos:
            if hasattr(sub_repo, name):
                return getattr(sub_repo, name)
        raise AttributeError(f"'{type(self).__name__}' object has no attribute '{name}'")

    def __dir__(self) -> List[str]:
        attrs = set(super().__dir__())
        for sub_repo in self._sub_repos:
            attrs.update(dir(sub_repo))
        return sorted(attrs)
