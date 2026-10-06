"""
Declarative database models package.
"""
from core.db.models.base import Base
from core.db.models.user import AppUser
from core.db.models.purchase import UserPurchase
from core.db.models.warehouse import (
    RawBlackFriday,
    BlackFridayCleaned,
    CustomerSegment,
    ProductNetworkMetric,
    CuratedProduct,
    UserCart,
)

__all__ = [
    "Base",
    "AppUser",
    "UserPurchase",
    "RawBlackFriday",
    "BlackFridayCleaned",
    "CustomerSegment",
    "ProductNetworkMetric",
    "CuratedProduct",
    "UserCart",
]

