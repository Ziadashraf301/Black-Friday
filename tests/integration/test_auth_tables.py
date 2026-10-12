"""
Regression test for Fix 6.2:
- Remove DDL calls (ensure_user_tables / create_app_tables) from register_user and authenticate_user
- Verify startup ensures all tables exist
- Verify fresh register -> login works end to end
- Capture SQL during login and assert NO DDL statements are executed
"""
import uuid
import pytest
from sqlalchemy import event
from core.db.repository import BlackFridayRepository
from apps.api.services.auth_service import auth_service


@pytest.fixture
def repo():
    r = BlackFridayRepository()
    r.create_app_tables()
    return r


def test_startup_creates_all_tables(repo):
    """Verify create_app_tables on startup initializes application and warehouse tables."""
    from sqlalchemy import inspect
    repo.create_app_tables()
    insp = inspect(repo.engine)
    for table_name in ["app_users", "user_purchases", "user_carts"]:
        assert insp.has_table(table_name), f"Expected table '{table_name}' to exist"


def test_auth_end_to_end_and_no_ddl_on_login(repo):
    """Verify register -> login works cleanly and executes ZERO DDL statements during authentication."""
    unique_email = f"user_{uuid.uuid4().hex[:8]}@example.com"
    user_data = {
        "name": "Test User",
        "email": unique_email,
        "password": "strongpassword123",
        "gender": "M",
        "age": "26-35",
        "city_category": "A",
        "marital_status": 0,
        "occupation": 4,
        "stay_in_current_city_years": "2",
    }

    # 1. Registration
    reg_result = auth_service.register_user(user_data, repo=repo)
    assert "access_token" in reg_result
    assert reg_result["user_id"] > 0

    # 2. Capture SQL during login via SQLAlchemy listener
    captured_statements = []

    def capture_sql(conn, cursor, statement, parameters, context, executemany):
        captured_statements.append(statement)

    event.listen(repo.engine, "before_cursor_execute", capture_sql)
    try:
        login_result = auth_service.authenticate_user(
            email=unique_email,
            password="strongpassword123",
            repo=repo,
        )
    finally:
        event.remove(repo.engine, "before_cursor_execute", capture_sql)

    # 3. Assert login succeeded
    assert "access_token" in login_result
    assert login_result["user_id"] == reg_result["user_id"]

    # 4. Assert queries were executed and NONE contain DDL
    assert len(captured_statements) > 0, "Expected database queries during login"
    ddl_keywords = ("CREATE TABLE", "ALTER TABLE", "DROP TABLE", "CREATE INDEX")
    for stmt in captured_statements:
        normalized_stmt = " ".join(stmt.strip().upper().split())
        for ddl_kw in ddl_keywords:
            assert ddl_kw not in normalized_stmt, f"DDL detected during login: {stmt}"


def test_register_executes_no_ddl(repo):
    """Verify registration also executes zero DDL statements."""
    unique_email = f"user_{uuid.uuid4().hex[:8]}@example.com"
    user_data = {
        "name": "No DDL Register",
        "email": unique_email,
        "password": "strongpassword123",
        "gender": "F",
        "age": "18-25",
        "city_category": "B",
        "marital_status": 1,
        "occupation": 2,
        "stay_in_current_city_years": "1",
    }

    captured_statements = []

    def capture_sql(conn, cursor, statement, parameters, context, executemany):
        captured_statements.append(statement)

    event.listen(repo.engine, "before_cursor_execute", capture_sql)
    try:
        reg_result = auth_service.register_user(user_data, repo=repo)
    finally:
        event.remove(repo.engine, "before_cursor_execute", capture_sql)

    assert "access_token" in reg_result
    ddl_keywords = ("CREATE TABLE", "ALTER TABLE", "DROP TABLE", "CREATE INDEX")
    for stmt in captured_statements:
        normalized_stmt = " ".join(stmt.strip().upper().split())
        for ddl_kw in ddl_keywords:
            assert ddl_kw not in normalized_stmt, f"DDL detected during register: {stmt}"
