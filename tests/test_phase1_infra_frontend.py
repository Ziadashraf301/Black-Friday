"""
Phase 1 Automated Test Suite (Task P1-08).
Validates 100% of Phase 1 Deliverables:
  - P1-01: pgvector & Hybrid Vector Schema (test_pgvector_schema)
  - P1-02: Audit Existing 20 Products & Assets (test_existing_20_products_audited)
  - P1-03: Complete 30 Catalog Products (test_curated_catalog_30_items)
  - P1-04: 30 Product Image Assets Coverage (test_product_image_paths_exist)
  - P1-05: Security & Redis Sliding-Window Rate Limiter (test_redis_rate_limit_5_min_20_day)
  - P1-06: Frontend Layer 1 Reflex Bot Drawer Auth & Chips (test_frontend_bot_auth_check)
  - P1-07: Ingestion & Incremental Vector Embeddings (test_catalog_embeddings_incremental_skip)
"""
import os
import json
import pytest
from pathlib import Path
from fastapi import HTTPException

from core.config import settings
from core.db.repository import BlackFridayRepository
from apps.api.rate_limiting.rate_limiter import RedisRateLimiter
from apps.api.services.rate_limiter_service import RateLimiterService
from ml.pipelines.seed_curated import CuratedCatalogSeeder


@pytest.fixture(scope="module")
def repo():
    return BlackFridayRepository()


@pytest.fixture
def catalog_data():
    json_path = settings.BASE_DIR / "data" / "curated_products.json"
    assert json_path.exists(), f"Catalog file not found at {json_path}"
    with open(json_path, "r", encoding="utf-8") as f:
        return json.load(f)


# ==============================================================================
# P1-01: pgvector & DB Schema Migration
# ==============================================================================
def test_pgvector_schema(repo):
    """Validates pgvector extension, vector(768), tsvector columns, and HNSW/GIN indexes."""
    with repo.engine.connect() as conn:
        from sqlalchemy import text
        # 1. Extension check
        ext_res = conn.execute(text("SELECT extname FROM pg_extension WHERE extname='vector';")).fetchall()
        assert len(ext_res) == 1, "pgvector extension is not enabled in PostgreSQL"

        # 2. Columns check
        col_res = conn.execute(text("""
            SELECT column_name, udt_name 
            FROM information_schema.columns 
            WHERE table_name='curated_products' AND column_name IN ('embedding', 'search_vector');
        """)).fetchall()
        cols = {row[0]: row[1] for row in col_res}
        assert "embedding" in cols, "embedding column missing in curated_products"
        assert cols["embedding"] == "vector", f"embedding column must be vector, got {cols['embedding']}"
        assert "search_vector" in cols, "search_vector column missing in curated_products"
        assert cols["search_vector"] == "tsvector", f"search_vector column must be tsvector, got {cols['search_vector']}"

        # 3. Index check
        idx_res = conn.execute(text("""
            SELECT indexname 
            FROM pg_indexes 
            WHERE tablename='curated_products';
        """)).fetchall()
        indexes = [row[0] for row in idx_res]
        assert "idx_curated_embedding_hnsw" in indexes, "HNSW vector index missing on curated_products"
        assert "idx_curated_search_vector" in indexes, "GIN fulltext index missing on curated_products"


# ==============================================================================
# P1-02: Audit First 20 Products & Asset Coverage
# ==============================================================================
def test_existing_20_products_audited(catalog_data):
    """Audits the first 20 products ensuring required fields and existing image assets."""
    required_fields = [
        "product_id", "name", "description", "category_name",
        "original_price", "discounted_price", "sizes", "image_url"
    ]
    first_20 = catalog_data[:20]
    assert len(first_20) == 20, "First 20 products must be present"

    for idx, p in enumerate(first_20):
        for field in required_fields:
            assert p.get(field) is not None, f"Product {idx+1} ({p.get('product_id')}) missing field '{field}'"
        assert len(p["sizes"]) > 0, f"Product {idx+1} sizes cannot be empty"
        assert p["discounted_price"] > 0, f"Product {idx+1} discounted_price must be positive"

        # Image existence in assets directory
        img_name = os.path.basename(p["image_url"])
        asset_file = settings.BASE_DIR / "apps" / "reflex_app" / "assets" / "products" / img_name
        assert asset_file.exists(), f"Image asset missing: {asset_file}"


# ==============================================================================
# P1-03: Complete Curated Catalog Products (30+ items up to 50)
# ==============================================================================
def test_curated_catalog_30_items(catalog_data):
    """Validates complete curated catalog, corrected items 21-23, and catalog expansion."""
    assert len(catalog_data) >= 30, f"Expected at least 30 products, got {len(catalog_data)}"

    product_ids = [p["product_id"] for p in catalog_data]
    assert len(set(product_ids)) == len(catalog_data), "All product IDs must be unique"

    # Validate items 21, 22, 23 (indices 20, 21, 22)
    p21 = catalog_data[20]
    assert p21["product_id"] == "P00278642"
    assert p21["product_category_1"] == 5, "Product 21 category_1 must be 5 (Knitwear)"
    assert len(p21["apriori_bundles"]) > 0, "Product 21 must have apriori bundles from network table"

    p22 = catalog_data[21]
    assert p22["product_id"] == "P00242742"
    assert p22["product_category_1"] == 1, "Product 22 category_1 must be 1"
    assert p22["product_category_2"] == 2
    assert p22["product_category_3"] == 9
    assert len(p22["apriori_bundles"]) >= 2, "Product 22 must have apriori bundle associations"

    p23 = catalog_data[22]
    assert p23["product_id"] == "P00034742"
    assert p23["product_category_1"] == 5, "Product 23 category_1 must be 5"
    assert p23["product_category_2"] == 14
    assert p23["product_category_3"] == 17

    # Validate diversity across the catalog items
    categories = set(p.get("category_name") for p in catalog_data)
    assert len(categories) >= 7, f"Expected diverse categories, found {len(categories)}"
    genders = set(p.get("gender") for p in catalog_data)
    assert {"Men", "Women", "Unisex"}.issubset(genders), "Catalog must cover Men, Women, and Unisex"


# ==============================================================================
# P1-04: Product Image Asset Coverage
# ==============================================================================
def test_product_image_paths_exist(catalog_data):
    """Verifies that all product images exist in Reflex assets and compiled web public directories."""
    assert len(catalog_data) >= 30
    dirs = [
        settings.BASE_DIR / "apps" / "reflex_app" / "assets" / "products",
        settings.BASE_DIR / "apps" / "reflex_app" / ".web" / "public" / "products",
    ]

    for p in catalog_data:
        img_name = os.path.basename(p["image_url"])
        for d in dirs:
            target = d / img_name
            assert target.exists(), f"Product image not found at {target}"


# ==============================================================================
# P1-05: Security & Redis Rate Limiter Implementation
# ==============================================================================
def test_redis_rate_limit_5_min_20_day():
    """Validates sliding window rate limiter enforcing 5 req/min and 20 req/day quotas."""
    test_user_id = "test_shopper_99999"
    limiter = RedisRateLimiter(minute_limit=5, day_limit=20)
    service = RateLimiterService(limiter=limiter)

    # 1. Reset user state
    service.reset(test_user_id)

    # 2. First 5 requests within a minute must pass
    for i in range(1, 6):
        allowed, msg, retry, headers = service.check_user(test_user_id)
        assert allowed is True, f"Request {i} should be allowed"
        assert headers.get("X-RateLimit-Limit-Minute") == "5"

    # 3. 6th request within the same minute must breach quota and return 429
    allowed, msg, retry, headers = service.check_user(test_user_id)
    assert allowed is False, "6th request within minute must be blocked"
    assert "Max 5 requests per minute" in msg
    assert int(headers.get("Retry-After", "0")) > 0

    with pytest.raises(HTTPException) as exc_info:
        service.enforce_user(test_user_id)
    assert exc_info.value.status_code == 429

    # 4. Test daily limit (simulate 20 req/day quota)
    daily_test_user = "test_shopper_daily_limit"
    daily_limiter = RedisRateLimiter(minute_limit=100, day_limit=20)
    daily_service = RateLimiterService(limiter=daily_limiter)
    daily_service.reset(daily_test_user)

    for i in range(1, 21):
        allowed, msg, retry, headers = daily_service.check_user(daily_test_user)
        assert allowed is True, f"Request {i} within day must be allowed"

    # 21st request exceeds daily quota
    allowed, msg, retry, headers = daily_service.check_user(daily_test_user)
    assert allowed is False, "21st request must exceed daily quota"
    assert "Max 20 requests per day" in msg

    # Clean up
    service.reset(test_user_id)
    daily_service.reset(daily_test_user)


# ==============================================================================
# P1-06: Frontend Layer 1 (Reflex Bot Drawer Auth Check)
# ==============================================================================
def test_frontend_bot_auth_check():
    """Validates Reflex ShoppingState bot drawer auth gating and preset action chips."""
    import sys
    sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))
    from reflex_app.state import ShoppingState
    from reflex_app.components.bot_drawer import bot_drawer, bot_trigger_button

    # 1. Unauthenticated test
    state = ShoppingState()
    state.auth_token = ""
    assert state.is_authenticated is False

    # Clicking trigger while unauthenticated must prompt login
    res = state.handle_bot_trigger_click()
    assert res is False
    assert state.show_auth is True, "Must prompt auth modal when guest clicks bot trigger"
    assert state.bot_auth_checked is True
    assert "sign in" in state.bot_auth_warning.lower()

    # 2. Authenticated test
    auth_state = ShoppingState()
    auth_state.auth_token = "mock_jwt_access_token_12345"
    auth_state.user_name = "Alex"
    auth_state.user_cluster_persona = "Urban Trendsetter"
    assert auth_state.is_authenticated is True

    opened = auth_state.handle_bot_trigger_click()
    assert opened is True
    assert auth_state.is_bot_open is True
    assert len(auth_state.bot_messages) > 0

    # 3. Action chips test (*"Top Deals Today"*, *"Sale Products"*)
    auth_state.click_action_chip("Top Deals Today")
    last_assistant_msg = [m for m in auth_state.bot_messages if m["role"] == "assistant"][-1]
    assert "deal" in last_assistant_msg["content"].lower()

    auth_state.click_action_chip("Sale Products")
    last_assistant_msg = [m for m in auth_state.bot_messages if m["role"] == "assistant"][-1]
    assert "sale" in last_assistant_msg["content"].lower()

    # 4. Component compilation / rendering check
    drawer_cmp = bot_drawer()
    trigger_cmp = bot_trigger_button()
    assert drawer_cmp is not None
    assert trigger_cmp is not None


# ==============================================================================
# P1-07: Ingestion & Incremental Vector Embedding Pipeline
# ==============================================================================
def test_catalog_embeddings_incremental_skip(repo):
    """Validates incremental embedding pipeline: skips existing non-null vectors."""
    # 1. Verify curated products exist in DB and all have non-null embeddings
    products = repo.get_curated_products()
    total_count = len(products)
    assert total_count >= 30, f"Expected at least 30 products in DB, got {total_count}"
    for p in products:
        assert p["has_embedding"] is True, f"Product {p['product_id']} missing embedding"

    # 2. Run incremental seeder
    seeder = CuratedCatalogSeeder(repo=repo)
    result = seeder.run(force_reembed=False)

    assert result["seeded_count"] == total_count
    assert result["embedded_count"] == 0, "Incremental skip should embed 0 items when all are present"
    assert result["skipped_count"] == total_count, f"Incremental skip should skip all {total_count} existing products"
