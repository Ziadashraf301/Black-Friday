"""Baseline: the warehouse schema of docker/postgres/init_schema.sql (round 2, CORE-16).

Every statement is IF NOT EXISTS, so this runs on an empty database, on one the Docker init script
already created, and on an existing warehouse without touching its rows. It is a frozen copy: change
the schema in a new revision, never here.

Revision ID: 0001
Revises:
Create Date: 2026-10-12
"""
from alembic import op

revision = "0001"
down_revision = None
branch_labels = None
depends_on = None

BASELINE_SQL = """
CREATE EXTENSION IF NOT EXISTS vector;

-- 1. Raw Staging Table (Exact 1:1 match with Train.csv, accepts nulls)
CREATE TABLE IF NOT EXISTS raw_black_friday (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    product_id VARCHAR(32) NOT NULL,
    gender VARCHAR(8) NOT NULL,
    age VARCHAR(16) NOT NULL,
    occupation INTEGER NOT NULL,
    city_category VARCHAR(4) NOT NULL,
    stay_in_current_city_years VARCHAR(8) NOT NULL,
    marital_status INTEGER NOT NULL,
    product_category_1 INTEGER NOT NULL,
    product_category_2 INTEGER NULL,
    product_category_3 INTEGER NULL,
    purchase NUMERIC(12, 2) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_raw_user_id ON raw_black_friday (user_id);
CREATE INDEX IF NOT EXISTS idx_raw_product_id ON raw_black_friday (product_id);
CREATE INDEX IF NOT EXISTS idx_raw_cat1 ON raw_black_friday (product_category_1);

-- 2. Cleaned & Imputed Transactions (missForest equivalent imputed categories)
CREATE TABLE IF NOT EXISTS black_friday_cleaned (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    product_id VARCHAR(32) NOT NULL,
    gender VARCHAR(8) NOT NULL,
    age VARCHAR(16) NOT NULL,
    occupation INTEGER NOT NULL,
    city_category VARCHAR(4) NOT NULL,
    stay_in_current_city_years VARCHAR(8) NOT NULL,
    marital_status INTEGER NOT NULL,
    product_category_1 INTEGER NOT NULL,
    product_category_2 INTEGER NOT NULL,
    product_category_3 INTEGER NOT NULL,
    purchase NUMERIC(12, 2) NOT NULL,
    is_outlier BOOLEAN NOT NULL DEFAULT FALSE,
    normalized_purchase NUMERIC(8, 6) NOT NULL,
    split VARCHAR(16) NOT NULL DEFAULT 'train',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_cleaned_user_id ON black_friday_cleaned (user_id);
CREATE INDEX IF NOT EXISTS idx_cleaned_product_id ON black_friday_cleaned (product_id);
CREATE INDEX IF NOT EXISTS idx_cleaned_categories
ON black_friday_cleaned (product_category_1, product_category_2, product_category_3);
CREATE INDEX IF NOT EXISTS idx_cleaned_is_outlier ON black_friday_cleaned (is_outlier);
CREATE INDEX IF NOT EXISTS idx_cleaned_split ON black_friday_cleaned (split);

-- 3. Customer Segments Table (RFM & Gower 10-Cluster Personas)
CREATE TABLE IF NOT EXISTS customer_segments (
    user_id INTEGER PRIMARY KEY,
    lifetime_value NUMERIC(14, 2) NOT NULL,
    average_order_value NUMERIC(10, 2) NOT NULL,
    frequency INTEGER NOT NULL,
    purchase_amount_variability NUMERIC(12, 2) NOT NULL,
    popular_category INTEGER NOT NULL,
    gender VARCHAR(8) NOT NULL,
    marital_status VARCHAR(16) NOT NULL,
    age_group VARCHAR(16) NOT NULL,
    age_binned VARCHAR(8) NOT NULL,
    cluster_id INTEGER NOT NULL,
    cluster_persona VARCHAR(128) NOT NULL,
    recommended_action TEXT,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_cust_cluster_id ON customer_segments (cluster_id);
CREATE INDEX IF NOT EXISTS idx_cust_ltv ON customer_segments (lifetime_value DESC);

-- 4. Product Association & Network Metrics (Apriori + Item2Vec + PageRank + HITS)
CREATE TABLE IF NOT EXISTS product_network_metrics (
    product_id VARCHAR(32) PRIMARY KEY,
    order_count INTEGER NOT NULL,
    pagerank_score NUMERIC(8, 5) NOT NULL,
    hub_score NUMERIC(8, 5) NOT NULL,
    authority_score NUMERIC(8, 5) NOT NULL,
    top_associated_product VARCHAR(32),
    highest_lift_rule NUMERIC(8, 4),
    top_bundle_recommendations JSONB,
    item2vec_recommendations JSONB,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_prod_pagerank ON product_network_metrics (pagerank_score DESC);
CREATE INDEX IF NOT EXISTS idx_prod_hub ON product_network_metrics (hub_score DESC);
CREATE INDEX IF NOT EXISTS idx_prod_authority ON product_network_metrics (authority_score DESC);

-- 5. Curated Products Table with pgvector (768-dim) and Full-Text Search tsvector
CREATE TABLE IF NOT EXISTS curated_products (
    product_id VARCHAR(32) PRIMARY KEY,
    name VARCHAR(255),
    title VARCHAR(255),
    tagline TEXT,
    description TEXT,
    category_name VARCHAR(100),
    category VARCHAR(100),
    gender VARCHAR(50),
    brand VARCHAR(100),
    style VARCHAR(100),
    season VARCHAR(50),
    badge VARCHAR(50),
    is_hero BOOLEAN NOT NULL DEFAULT FALSE,
    sizes JSONB,
    rating FLOAT NOT NULL DEFAULT 4.5,
    review_count INTEGER NOT NULL DEFAULT 100,
    original_price FLOAT NOT NULL DEFAULT 99.9,
    discounted_price FLOAT NOT NULL DEFAULT 49.9,
    image_url TEXT,
    order_count INTEGER NOT NULL DEFAULT 0,
    product_category_1 INTEGER NOT NULL DEFAULT 1,
    product_category_2 INTEGER,
    product_category_3 INTEGER,
    apriori_bundles JSONB,
    item2vec_similars JSONB,
    embedding vector(768),
    search_vector tsvector
);

CREATE INDEX IF NOT EXISTS idx_curated_embedding_hnsw ON curated_products USING hnsw (embedding vector_cosine_ops);
CREATE INDEX IF NOT EXISTS idx_curated_search_vector ON curated_products USING gin (search_vector);

-- 6. Application Users Table
CREATE TABLE IF NOT EXISTS app_users (
    user_id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    gender VARCHAR(1),
    age VARCHAR(10),
    city_category VARCHAR(1),
    marital_status INTEGER,
    occupation INTEGER,
    stay_in_current_city_years VARCHAR(5),
    cluster_id INTEGER,
    cluster_persona TEXT,
    recommended_action TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_users_email ON app_users (email);

-- 7. User Purchases Table
CREATE TABLE IF NOT EXISTS user_purchases (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES app_users(user_id) ON DELETE CASCADE,
    product_id TEXT NOT NULL,
    product_category_1 INTEGER,
    product_category_2 INTEGER,
    product_category_3 INTEGER,
    predicted_usd FLOAT,
    model_used TEXT,
    purchased_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_purchases_user_id ON user_purchases (user_id);

-- 8. Cold-Tier User Carts Table
CREATE TABLE IF NOT EXISTS user_carts (
    user_id VARCHAR(64) PRIMARY KEY,
    session_id VARCHAR(64) NOT NULL,
    cart_data JSONB NOT NULL,
    item_count INTEGER DEFAULT 0,
    total_amount NUMERIC(10, 2) DEFAULT 0.00,
    updated_at VARCHAR(64)
);

-- 9. Semantic Query Cache Table with pgvector HNSW index
CREATE TABLE IF NOT EXISTS semantic_query_cache (
    id BIGSERIAL PRIMARY KEY,
    query_text TEXT NOT NULL,
    embedding vector(768) NOT NULL,
    response_text TEXT NOT NULL,
    ui_payload JSONB NOT NULL,
    intent VARCHAR(64) NOT NULL,
    user_key TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Older databases created this table before the per-user key existed.
ALTER TABLE semantic_query_cache ADD COLUMN IF NOT EXISTS user_key TEXT;

CREATE INDEX IF NOT EXISTS idx_semantic_cache_hnsw
ON semantic_query_cache USING hnsw (embedding vector_cosine_ops);

CREATE INDEX IF NOT EXISTS idx_semantic_cache_user_created
ON semantic_query_cache (user_key, created_at);
"""


def upgrade() -> None:
    # exec_driver_sql: the comments contain ':' ("1:1"), which text() would read as a bind parameter.
    op.get_bind().exec_driver_sql(BASELINE_SQL)


def downgrade() -> None:
    raise NotImplementedError("The baseline holds the warehouse data; drop the tables by hand if you really mean it.")
