"""
Regression test for Fix 4.1:
- Parameterized :top_k query binding and injection defense
- No inline DDL (CREATE TABLE / CREATE INDEX / create_all) executed during get_curated_products or cart operations
"""
import pytest
from sqlalchemy import event
from core.db.repositories.warehouse_repo import WarehouseRepository


def test_malicious_top_k_is_safely_rejected():
    """Verify malicious top_k string cannot cause SQL injection."""
    repo = WarehouseRepository()
    malicious_inputs = [
        "5; DROP TABLE curated_products; --",
        "1 UNION SELECT * FROM users",
        "' OR '1'='1",
    ]
    for mal_input in malicious_inputs:
        with pytest.raises(ValueError):
            repo._execute_rrf_query(
                query_vec_str="[0.0]",
                query_text="jacket",
                top_k=mal_input,
            )


def test_no_ddl_issued_during_get_curated_and_cart_calls():
    """Verify that reading products or saving/loading carts issues zero DDL statements."""
    repo = WarehouseRepository()
    engine = repo.engine

    executed_statements = []

    def before_cursor_execute_listener(conn, cursor, statement, parameters, context, executemany):
        executed_statements.append(statement.strip().upper())

    event.listen(engine, "before_cursor_execute", before_cursor_execute_listener)

    try:
        # 1. Read curated products
        repo.get_curated_products()

        # 2. Save user cart
        repo.save_user_cart_snapshot(
            user_id="test_perf_user_1",
            session_id="session_perf_1",
            cart_dict={"item_count": 2, "final_total": 85.50}
        )

        # 3. Load user cart
        repo.load_user_cart_snapshot(user_id="test_perf_user_1")

        # Assert no DDL statements were issued
        ddl_keywords = ["CREATE TABLE", "CREATE INDEX", "ALTER TABLE", "DROP TABLE"]
        ddl_executed = [
            stmt for stmt in executed_statements
            if any(stmt.startswith(kw) or f" {kw} " in stmt for kw in ddl_keywords)
        ]

        assert len(ddl_executed) == 0, f"Unexpected DDL statements detected: {ddl_executed}"

    finally:
        event.remove(engine, "before_cursor_execute", before_cursor_execute_listener)
