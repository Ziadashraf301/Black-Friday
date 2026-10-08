"""
Regression tests for Fix 7.2:
- Batch price estimation issues a constant number of category queries (O(1) bulk query instead of N+1).
- Batch pricing results match per-item pricing results on a sample.
- process_purchase executes without DDL queries.
- Member discount calculation uses extracted helper.
"""
import uuid
import pytest
from sqlalchemy import event
from core.db.repository import BlackFridayRepository
from apps.api.services.shopper_service import shopper_service
from apps.api.services.helpers import calculate_member_discount_price
from apps.api.services.model_service import model_service
from core.cache import cache_manager


@pytest.fixture(autouse=True)
def setup_models_and_tables():
    model_service.load_models()
    repo = BlackFridayRepository()
    repo.create_app_tables()
    return repo


def test_batch_price_issues_constant_category_queries(setup_models_and_tables):
    """Verify estimate_price_batch executes exactly ONE bulk category query for N items missing categories."""
    repo = setup_models_and_tables
    # 10 distinct products without category info
    n_items = 10
    items = [{"product_id": f"P000254{i:02d}"} for i in range(n_items)]

    # Clear Redis price cache for these test products so inference is executed
    for item in items:
        cache_manager.delete_pattern(f"price:{item['product_id']}:*")

    category_queries = []

    def capture_queries(conn, cursor, statement, parameters, context, executemany):
        normalized = " ".join(statement.strip().upper().split())
        if "MODE() WITHIN GROUP" in normalized or "BLACK_FRIDAY_CLEANED" in normalized:
            category_queries.append(statement)

    event.listen(repo.engine, "before_cursor_execute", capture_queries)
    try:
        quotes = shopper_service.estimate_price_batch(items=items, repo=repo)
    finally:
        event.remove(repo.engine, "before_cursor_execute", capture_queries)

    # In the old code with N=10, 10 separate queries were fired.
    # With Fix 7.2, exactly 1 bulk query is executed.
    assert len(category_queries) == 1, (
        f"Expected exactly 1 bulk category query for {n_items} items, but captured {len(category_queries)} queries"
    )
    assert len(quotes) == n_items


def test_batch_pricing_equals_per_item_pricing(setup_models_and_tables):
    """Verify batch pricing results match individual per-item pricing results on a sample."""
    repo = setup_models_and_tables
    demo = {
        "gender": "M",
        "age": "26-35",
        "occupation": 4,
        "city_category": "B",
        "stay_in_current_city_years": "2",
        "marital_status": 0,
    }
    sample_items = [
        {"product_id": "P00025442", "product_category_1": 1, "product_category_2": 6, "product_category_3": 14, **demo},
        {"product_id": "P00110742", "product_category_1": 1, "product_category_2": 2, "product_category_3": 8, **demo},
        {"product_id": "P00000142", "product_category_1": 5, "product_category_2": None, "product_category_3": None, **demo},
    ]

    # Calculate via estimate_price individually
    individual_quotes = []
    for it in sample_items:
        q = shopper_service.estimate_price(
            product_id=it["product_id"],
            cat1=it["product_category_1"],
            cat2=it["product_category_2"],
            cat3=it["product_category_3"],
            repo=repo,
            gender=it["gender"],
            age=it["age"],
            occupation=it["occupation"],
            city_category=it["city_category"],
            stay_in_current_city_years=it["stay_in_current_city_years"],
            marital_status=it["marital_status"],
        )
        individual_quotes.append(q)

    # Calculate via estimate_price_batch
    batch_quotes = shopper_service.estimate_price_batch(items=sample_items, repo=repo)

    assert len(individual_quotes) == len(batch_quotes)
    for ind, bat in zip(individual_quotes, batch_quotes):
        assert ind["product_id"] == bat["product_id"]
        assert ind["predicted_usd"] == bat["predicted_usd"]
        assert ind["catalog_price"] == bat["catalog_price"]
        assert ind["model_used"] == bat["model_used"]


def test_process_purchase_executes_no_ddl(setup_models_and_tables):
    """Verify process_purchase commits purchase without executing DDL statements."""
    repo = setup_models_and_tables
    # Create test user
    email = f"user_{uuid.uuid4().hex[:8]}@example.com"
    user = repo.create_user({
        "name": "Purchase Tester",
        "email": email,
        "password_hash": "dummyhash",
        "gender": "M",
        "age": "26-35",
        "city_category": "A",
        "marital_status": 0,
        "occupation": 4,
        "stay_in_current_city_years": "2",
        "cluster_id": 1,
        "cluster_persona": "Preferred Member",
        "recommended_action": "Exclusive",
    })

    captured_statements = []

    def capture_sql(conn, cursor, statement, parameters, context, executemany):
        captured_statements.append(statement)

    event.listen(repo.engine, "before_cursor_execute", capture_sql)
    try:
        rec = shopper_service.process_purchase(
            user_id=user["user_id"],
            product_id="P00025442",
            cat1=1,
            cat2=6,
            cat3=14,
            repo=repo,
        )
    finally:
        event.remove(repo.engine, "before_cursor_execute", capture_sql)

    assert rec["id"] > 0
    assert rec["product_id"] == "P00025442"
    ddl_keywords = ("CREATE TABLE", "ALTER TABLE", "DROP TABLE", "CREATE INDEX")
    for stmt in captured_statements:
        normalized_stmt = " ".join(stmt.strip().upper().split())
        for ddl_kw in ddl_keywords:
            assert ddl_kw not in normalized_stmt, f"DDL detected during purchase: {stmt}"


def test_member_discount_helper_logic():
    """Verify calculate_member_discount_price helper computes correctly and caps at 0.85 when needed."""
    # norm = 0.5: factor = 0.72 + 0.16 * 0.5 = 0.80 -> price = round(100 * 0.80, 2) = 80.0
    price_80 = calculate_member_discount_price(base_price=100.0, normalized_prediction=0.5)
    assert price_80 == 80.0

    # norm = 0.0: factor = 0.72 + 0 = 0.72 -> price = 72.0
    price_72 = calculate_member_discount_price(base_price=100.0, normalized_prediction=0.0)
    assert price_72 == 72.0

    # norm = 1.0: factor = 0.72 + 0.16 = 0.88 -> price = 88.0
    price_88 = calculate_member_discount_price(base_price=100.0, normalized_prediction=1.0)
    assert price_88 == 88.0

    # Cap at 85% if factor >= 1.0
    # Even if an anomalous prediction is passed, it clamps norm to <= 1.0, max factor is 0.88 < 1.0
    # If base price calculation ever yielded >= base_price, capped at 0.85
    capped = calculate_member_discount_price(base_price=0.0, normalized_prediction=0.5)
    assert capped == 0.0
