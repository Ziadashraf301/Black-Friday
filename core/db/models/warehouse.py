"""
SQLAlchemy ORM models for warehouse tables (raw, cleaned, segments, network metrics).
"""
from sqlalchemy import Column, Integer, BigInteger, String, Float, Boolean, Text, JSON
from sqlalchemy.dialects.postgresql import TSVECTOR
from pgvector.sqlalchemy import Vector
from core.db.models.base import Base


class RawBlackFriday(Base):
    """Raw ingested transactions."""
    __tablename__ = "raw_black_friday"

    id = Column(BigInteger, primary_key=True, autoincrement=True)
    user_id = Column(Integer, nullable=False)
    product_id = Column(String(32), nullable=False)
    gender = Column(String(1), nullable=False)
    age = Column(String(16), nullable=False)
    occupation = Column(Integer, nullable=False)
    city_category = Column(String(1), nullable=False)
    stay_in_current_city_years = Column(String(8), nullable=False)
    marital_status = Column(Integer, nullable=False)
    product_category_1 = Column(Integer, nullable=False)
    product_category_2 = Column(Integer, nullable=True)
    product_category_3 = Column(Integer, nullable=True)
    purchase = Column(Float, nullable=False)


class BlackFridayCleaned(Base):
    """Imputed and preprocessed transactions."""
    __tablename__ = "black_friday_cleaned"

    id = Column(BigInteger, primary_key=True, autoincrement=True)
    user_id = Column(Integer, nullable=False)
    product_id = Column(String(32), nullable=False, index=True)
    gender = Column(String(1), nullable=False)
    age = Column(String(16), nullable=False)
    occupation = Column(Integer, nullable=False)
    city_category = Column(String(1), nullable=False)
    stay_in_current_city_years = Column(String(8), nullable=False)
    marital_status = Column(Integer, nullable=False)
    product_category_1 = Column(Integer, nullable=False)
    product_category_2 = Column(Integer, nullable=False)
    product_category_3 = Column(Integer, nullable=False)
    purchase = Column(Float, nullable=False)
    is_outlier = Column(Boolean, nullable=False, default=False)
    normalized_purchase = Column(Float, nullable=False)
    split = Column(String(16), nullable=False, default="train")


class CustomerSegment(Base):
    """Aggregated customer personas from Gower clustering."""
    __tablename__ = "customer_segments"

    user_id = Column(Integer, primary_key=True)
    gender = Column(String(1))
    marital_status = Column(String(16))
    age_binned = Column(String(16))
    lifetime_value = Column(Float)
    average_order_value = Column(Float)
    frequency = Column(Integer)
    cluster_id = Column(Integer)
    cluster_persona = Column(Text)
    recommended_action = Column(Text)


class ProductNetworkMetric(Base):
    """Product graph centrality and bundle recommendations."""
    __tablename__ = "product_network_metrics"

    product_id = Column(String(32), primary_key=True)
    order_count = Column(Integer, nullable=False)
    pagerank_score = Column(Float, nullable=False)
    hub_score = Column(Float, nullable=False)
    authority_score = Column(Float, nullable=False)
    top_associated_product = Column(String(32), nullable=True)
    highest_lift_rule = Column(Float, nullable=True)
    top_bundle_recommendations = Column(JSON, nullable=True)
    item2vec_recommendations = Column(JSON, nullable=True)


class CuratedProduct(Base):
    """Seeded database table for curated catalog products."""
    __tablename__ = "curated_products"

    product_id = Column(String(32), primary_key=True)
    name = Column(String(255), nullable=True)
    title = Column(String(255), nullable=True)
    tagline = Column(Text, nullable=True)
    description = Column(Text, nullable=True)
    category_name = Column(String(100), nullable=True)
    category = Column(String(100), nullable=True)
    gender = Column(String(50), nullable=True)
    brand = Column(String(100), nullable=True)
    style = Column(String(100), nullable=True)
    season = Column(String(50), nullable=True)
    badge = Column(String(50), nullable=True)
    is_hero = Column(Boolean, nullable=False, default=False)
    sizes = Column(JSON, nullable=True)
    rating = Column(Float, nullable=False, default=4.5)
    review_count = Column(Integer, nullable=False, default=100)
    original_price = Column(Float, nullable=False, default=99.9)
    discounted_price = Column(Float, nullable=False, default=49.9)
    image_url = Column(Text, nullable=True)
    order_count = Column(Integer, nullable=False, default=0)
    product_category_1 = Column(Integer, nullable=False, default=1)
    product_category_2 = Column(Integer, nullable=True)
    product_category_3 = Column(Integer, nullable=True)
    apriori_bundles = Column(JSON, nullable=True)
    item2vec_similars = Column(JSON, nullable=True)
    embedding = Column(Vector(768), nullable=True)
    search_vector = Column(TSVECTOR, nullable=True)


class UserCart(Base):
    """Cold-tier durable cart snapshots (Phase 4 - Task P4-06)."""
    __tablename__ = "user_carts"

    user_id = Column(String(64), primary_key=True)
    session_id = Column(String(64), nullable=False)
    cart_data = Column(JSON, nullable=False)
    item_count = Column(Integer, default=0)
    total_amount = Column(Float, default=0.0)
    updated_at = Column(String(64), nullable=True)



