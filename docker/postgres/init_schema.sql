-- ==============================================================================
-- BLACK FRIDAY ANALYTICAL DATA WAREHOUSE SCHEMA (PostgreSQL 16)
-- Target Database: fridayblack
-- ==============================================================================

\connect fridayblack;

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
    is_outlier BOOLEAN DEFAULT FALSE,
    normalized_purchase NUMERIC(8, 6),
    split VARCHAR(16) NOT NULL DEFAULT 'train',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_cleaned_user_id ON black_friday_cleaned (user_id);
CREATE INDEX IF NOT EXISTS idx_cleaned_product_id ON black_friday_cleaned (product_id);
CREATE INDEX IF NOT EXISTS idx_cleaned_categories ON black_friday_cleaned (product_category_1, product_category_2, product_category_3);
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

