"""
SQLAlchemy ORM model for application users.
"""
from sqlalchemy import Column, Integer, String, Text, DateTime, func
from core.db.models.base import Base


class AppUser(Base):
    """Registered application user and shopper demographic profile."""
    __tablename__ = "app_users"

    user_id = Column(Integer, primary_key=True, autoincrement=True)
    name = Column(Text, nullable=False)
    email = Column(Text, unique=True, nullable=False)
    password_hash = Column(Text, nullable=False)
    gender = Column(String(1))
    age = Column(String(10))
    city_category = Column(String(1))
    marital_status = Column(Integer)
    occupation = Column(Integer)
    stay_in_current_city_years = Column(String(5))
    cluster_id = Column(Integer)
    cluster_persona = Column(Text)
    recommended_action = Column(Text)
    created_at = Column(DateTime(timezone=True), server_default=func.now())
