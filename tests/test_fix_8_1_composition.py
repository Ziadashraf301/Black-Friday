"""
Regression test for Fix 8.1:
- Transition BlackFridayRepository from multiple inheritance to composition
- Verify domain repository sub-instances are exposed
- Verify every public method of the old facade resolves and is callable
"""
import pytest
from core.db.repository import BlackFridayRepository
from core.db.repositories.base import BaseRepository
from core.db.repositories.warehouse_repo import WarehouseRepository
from core.db.repositories.analytics_repo import AnalyticsRepository
from core.db.repositories.segmentation_repo import SegmentationRepository
from core.db.repositories.recommendation_repo import RecommendationRepository
from core.db.repositories.user_repo import UserRepository


def test_black_friday_repository_composition_structure():
    """Verify BlackFridayRepository uses composition and aggregates all domain repositories."""
    repo = BlackFridayRepository()

    # Verify domain sub-repositories are instantiated
    assert isinstance(repo.warehouse, WarehouseRepository)
    assert isinstance(repo.analytics, AnalyticsRepository)
    assert isinstance(repo.segmentation, SegmentationRepository)
    assert isinstance(repo.recommendation, RecommendationRepository)
    assert isinstance(repo.users, UserRepository)


def test_every_public_method_of_old_facade_resolves():
    """Verify every public method across all 5 domain repositories still resolves on BlackFridayRepository."""
    repo = BlackFridayRepository()
    domain_classes = [
        BaseRepository,
        WarehouseRepository,
        AnalyticsRepository,
        SegmentationRepository,
        RecommendationRepository,
        UserRepository,
    ]

    all_public_methods = set()
    for cls in domain_classes:
        for attr_name in dir(cls):
            if not attr_name.startswith("_"):
                attr = getattr(cls, attr_name)
                if callable(attr):
                    all_public_methods.add(attr_name)

    # Note: create_app_tables in UserRepository was renamed to ensure_user_tables in Fix 3.1
    # Both create_app_tables (from BaseRepository) and ensure_user_tables (from UserRepository) must exist!
    assert "create_app_tables" in all_public_methods
    assert "ensure_user_tables" in all_public_methods

    missing_methods = []
    for method_name in all_public_methods:
        if not hasattr(repo, method_name):
            missing_methods.append(method_name)
        else:
            resolved = getattr(repo, method_name)
            assert callable(resolved), f"Attribute {method_name} is not callable on facade"

    assert len(missing_methods) == 0, f"Facade is missing delegated methods: {missing_methods}"
