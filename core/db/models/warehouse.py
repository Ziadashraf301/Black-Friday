"""
SQLAlchemy ORM models for warehouse tables (raw, cleaned, segments, network metrics, catalog, carts, semantic cache).

These models are the schema: they match docker/postgres/init_schema.sql and the Alembic migrations in
core/db/migrations (tests/integration/test_migrations.py compares them against a migrated database).
"""

from pgvector.sqlalchemy import Vector
from sqlalchemy import (
    BigInteger,
    Boolean,
    Column,
    DateTime,
    Float,
    Index,
    Integer,
    Numeric,
    String,
    Text,
    func,
    text,
)
from sqlalchemy.dialects.postgresql import JSONB, TSVECTOR

from core.db.models.base import Base


class RawBlackFriday(Base):
    """Raw ingested transactions."""

    __tablename__ = "raw_black_friday"
    __table_args__ = (
        Index("idx_raw_user_id", "user_id"),
        Index("idx_raw_product_id", "product_id"),
        Index("idx_raw_cat1", "product_category_1"),
    )

    id = Column(Integer, primary_key=True, autoincrement=True)
    user_id = Column(Integer, nullable=False)
    product_id = Column(String(32), nullable=False)
    gender = Column(String(8), nullable=False)
    age = Column(String(16), nullable=False)
    occupation = Column(Integer, nullable=False)
    city_category = Column(String(4), nullable=False)
    stay_in_current_city_years = Column(String(8), nullable=False)
    marital_status = Column(Integer, nullable=False)
    product_category_1 = Column(Integer, nullable=False)
    product_category_2 = Column(Integer, nullable=True)
    product_category_3 = Column(Integer, nullable=True)
    purchase = Column(Numeric(12, 2), nullable=False)
    created_at = Column(DateTime(timezone=True), server_default=func.now())


class BlackFridayCleaned(Base):
    """Imputed and preprocessed transactions."""

    __tablename__ = "black_friday_cleaned"
    __table_args__ = (
        Index("idx_cleaned_user_id", "user_id"),
        Index("idx_cleaned_product_id", "product_id"),
        Index("idx_cleaned_categories", "product_category_1", "product_category_2", "product_category_3"),
        Index("idx_cleaned_is_outlier", "is_outlier"),
        Index("idx_cleaned_split", "split"),
    )

    id = Column(Integer, primary_key=True, autoincrement=True)
    user_id = Column(Integer, nullable=False)
    product_id = Column(String(32), nullable=False)
    gender = Column(String(8), nullable=False)
    age = Column(String(16), nullable=False)
    occupation = Column(Integer, nullable=False)
    city_category = Column(String(4), nullable=False)
    stay_in_current_city_years = Column(String(8), nullable=False)
    marital_status = Column(Integer, nullable=False)
    product_category_1 = Column(Integer, nullable=False)
    product_category_2 = Column(Integer, nullable=False)
    product_category_3 = Column(Integer, nullable=False)
    purchase = Column(Numeric(12, 2), nullable=False)
    is_outlier = Column(Boolean, nullable=False, default=False, server_default=text("false"))
    normalized_purchase = Column(Numeric(8, 6), nullable=False)
    split = Column(String(16), nullable=False, default="train", server_default="train")
    created_at = Column(DateTime(timezone=True), server_default=func.now())


class CustomerSegment(Base):
    """Aggregated customer personas from Gower clustering (columns written by ml/pipelines/segmentation)."""

    __tablename__ = "customer_segments"
    __table_args__ = (
        Index("idx_cust_cluster_id", "cluster_id"),
        Index("idx_cust_ltv", text("lifetime_value DESC")),
    )

    user_id = Column(Integer, primary_key=True, autoincrement=False)
    lifetime_value = Column(Numeric(14, 2), nullable=False)
    average_order_value = Column(Numeric(10, 2), nullable=False)
    frequency = Column(Integer, nullable=False)
    purchase_amount_variability = Column(Numeric(12, 2), nullable=False)
    popular_category = Column(Integer, nullable=False)
    gender = Column(String(8), nullable=False)
    marital_status = Column(String(16), nullable=False)
    age_group = Column(String(16), nullable=False)
    age_binned = Column(String(8), nullable=False)
    cluster_id = Column(Integer, nullable=False)
    cluster_persona = Column(String(128), nullable=False)
    recommended_action = Column(Text)
    updated_at = Column(DateTime(timezone=True), server_default=func.now())


class ProductNetworkMetric(Base):
    """Product graph centrality and bundle recommendations."""

    __tablename__ = "product_network_metrics"
    __table_args__ = (
        Index("idx_prod_pagerank", text("pagerank_score DESC")),
        Index("idx_prod_hub", text("hub_score DESC")),
        Index("idx_prod_authority", text("authority_score DESC")),
    )

    product_id = Column(String(32), primary_key=True)
    order_count = Column(Integer, nullable=False)
    pagerank_score = Column(Numeric(8, 5), nullable=False)
    hub_score = Column(Numeric(8, 5), nullable=False)
    authority_score = Column(Numeric(8, 5), nullable=False)
    top_associated_product = Column(String(32), nullable=True)
    highest_lift_rule = Column(Numeric(8, 4), nullable=True)
    top_bundle_recommendations = Column(JSONB, nullable=True)
    item2vec_recommendations = Column(JSONB, nullable=True)
    updated_at = Column(DateTime(timezone=True), server_default=func.now())


class CuratedProduct(Base):
    """Seeded database table for curated catalog products."""

    __tablename__ = "curated_products"
    __table_args__ = (
        Index(
            "idx_curated_embedding_hnsw",
            "embedding",
            postgresql_using="hnsw",
            postgresql_ops={"embedding": "vector_cosine_ops"},
        ),
        Index("idx_curated_search_vector", "search_vector", postgresql_using="gin"),
    )

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
    is_hero = Column(Boolean, nullable=False, default=False, server_default=text("false"))
    sizes = Column(JSONB, nullable=True)
    rating = Column(Float, nullable=False, default=4.5, server_default=text("4.5"))
    review_count = Column(Integer, nullable=False, default=100, server_default=text("100"))
    original_price = Column(Numeric(10, 2), nullable=False)
    discounted_price = Column(Numeric(10, 2), nullable=False)
    image_url = Column(Text, nullable=True)
    order_count = Column(Integer, nullable=False, default=0, server_default=text("0"))
    product_category_1 = Column(Integer, nullable=False, default=1, server_default=text("1"))
    product_category_2 = Column(Integer, nullable=True)
    product_category_3 = Column(Integer, nullable=True)
    apriori_bundles = Column(JSONB, nullable=True)
    item2vec_similars = Column(JSONB, nullable=True)
    embedding = Column(Vector(768), nullable=True)
    search_vector = Column(TSVECTOR, nullable=True)


class UserCart(Base):
    """Cold-tier durable cart snapshots (Phase 4 - Task P4-06)."""

    __tablename__ = "user_carts"

    user_id = Column(String(64), primary_key=True)
    session_id = Column(String(64), nullable=False)
    cart_data = Column(JSONB, nullable=False)
    item_count = Column(Integer, default=0, server_default=text("0"))
    total_amount = Column(Numeric(10, 2), default=0.0, server_default=text("0.00"))
    updated_at = Column(DateTime(timezone=True), server_default=func.now())


class SemanticQueryCache(Base):
    """pgvector semantic cache of bot answers, keyed per user (written by WarehouseRepository)."""

    __tablename__ = "semantic_query_cache"
    __table_args__ = (
        Index(
            "idx_semantic_cache_hnsw",
            "embedding",
            postgresql_using="hnsw",
            postgresql_ops={"embedding": "vector_cosine_ops"},
        ),
        Index("idx_semantic_cache_user_created", "user_key", "created_at"),
    )

    id = Column(BigInteger, primary_key=True, autoincrement=True)
    query_text = Column(Text, nullable=False)
    embedding = Column(Vector(768), nullable=False)
    response_text = Column(Text, nullable=False)
    ui_payload = Column(JSONB, nullable=False)
    intent = Column(String(64), nullable=False)
    user_key = Column(Text)
    created_at = Column(DateTime(timezone=True), server_default=func.now())
