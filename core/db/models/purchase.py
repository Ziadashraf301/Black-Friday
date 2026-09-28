"""
SQLAlchemy ORM model for user purchase history.
"""
from sqlalchemy import Column, Integer, Float, Text, DateTime, ForeignKey, func
from core.db.models.base import Base


class UserPurchase(Base):
    """Historical purchase and model prediction log for a shopper."""
    __tablename__ = "user_purchases"

    id = Column(Integer, primary_key=True, autoincrement=True)
    user_id = Column(Integer, ForeignKey("app_users.user_id", ondelete="CASCADE"), nullable=False)
    product_id = Column(Text, nullable=False)
    product_category_1 = Column(Integer)
    product_category_2 = Column(Integer)
    product_category_3 = Column(Integer)
    predicted_usd = Column(Float)
    model_used = Column(Text)
    purchased_at = Column(DateTime(timezone=True), server_default=func.now())
