"""
SQLAlchemy ORM model for user purchase history.
"""

from sqlalchemy import Column, DateTime, ForeignKey, Index, Integer, Numeric, String, Text, func

from core.db.models.base import Base


class UserPurchase(Base):
    """Historical purchase and model prediction log for a shopper."""

    __tablename__ = "user_purchases"
    __table_args__ = (
        Index("idx_purchases_user_id", "user_id"),
        Index("idx_purchases_product_id", "product_id"),
    )

    id = Column(Integer, primary_key=True, autoincrement=True)
    user_id = Column(Integer, ForeignKey("app_users.user_id", ondelete="CASCADE"), nullable=False)
    # Dataset customers (customer_segments.user_id) and store accounts (this user_id) are separate id spaces
    product_id = Column(
        String(32), ForeignKey("curated_products.product_id", name="fk_user_purchases_product_id"), nullable=False
    )
    product_category_1 = Column(Integer)
    product_category_2 = Column(Integer)
    product_category_3 = Column(Integer)
    predicted_usd = Column(Numeric(10, 2))
    model_used = Column(Text)
    purchased_at = Column(DateTime(timezone=True), server_default=func.now())
