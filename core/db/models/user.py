"""
SQLAlchemy ORM model for application users.
"""

from sqlalchemy import Column, DateTime, Index, Integer, String, Text, func

from core.db.models.base import Base


class AppUser(Base):
    """Registered application user and shopper demographic profile."""

    __tablename__ = "app_users"
    __table_args__ = (Index("idx_users_email", "email"),)

    user_id = Column(Integer, primary_key=True, autoincrement=True)
    name = Column(Text, nullable=False)
    email = Column(Text, unique=True, nullable=False)
    password_hash = Column(Text, nullable=False)
    gender = Column(String(8))
    age = Column(String(10))
    city_category = Column(String(4))
    marital_status = Column(Integer)
    occupation = Column(Integer)
    stay_in_current_city_years = Column(String(5))
    cluster_id = Column(Integer)
    cluster_persona = Column(Text)
    recommended_action = Column(Text)
    created_at = Column(DateTime(timezone=True), server_default=func.now())
