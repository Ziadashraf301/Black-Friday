"""CORE-13: init_schema.sql and ensure_user_tables must never be looser than the ORM models on NOT NULL."""

from pathlib import Path

import pytest
from sqlalchemy import create_engine, text

from core.config import settings
from core.db.models import Base
from core.db.repositories.user_repo import UserRepository

SCHEMA_SQL = Path(__file__).resolve().parents[2] / "docker" / "postgres" / "init_schema.sql"
SCRATCH = "core13_schema_scratch"
NULLABLE_QUERY = text(
    "SELECT table_name, column_name, is_nullable FROM information_schema.columns WHERE table_schema = 'public'"
)


@pytest.fixture
def scratch_engine():
    base = settings.database_url.rsplit("/", 1)[0]
    admin = create_engine(base + "/postgres", isolation_level="AUTOCOMMIT")
    try:
        with admin.connect() as conn:
            conn.execute(text("SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = :n"), {"n": SCRATCH})
            conn.execute(text(f"DROP DATABASE IF EXISTS {SCRATCH}"))
            conn.execute(text(f"CREATE DATABASE {SCRATCH}"))
    except Exception as exc:
        pytest.skip(f"cannot create scratch database: {exc}")
    engine = create_engine(f"{base}/{SCRATCH}")
    yield engine
    engine.dispose()
    with admin.connect() as conn:
        conn.execute(text(f"DROP DATABASE IF EXISTS {SCRATCH}"))
    admin.dispose()


def _nullable(engine):
    with engine.connect() as conn:
        return {(t, c): n == "YES" for t, c, n in conn.execute(NULLABLE_QUERY)}


def test_init_schema_enforces_every_orm_not_null(scratch_engine):
    raw = scratch_engine.raw_connection()
    raw.cursor().execute(SCHEMA_SQL.read_text(encoding="utf-8"))
    raw.commit()
    raw.close()

    actual = _nullable(scratch_engine)
    loose = [
        f"{table.name}.{col.name}"
        for table in Base.metadata.sorted_tables
        for col in table.columns
        if not col.nullable and not col.primary_key and actual.get((table.name, col.name)) is True
    ]
    assert loose == []


def test_ensure_user_tables_requires_purchase_user(scratch_engine):
    repo = UserRepository(engine=scratch_engine)
    repo.ensure_user_tables()
    assert _nullable(scratch_engine)[("user_purchases", "user_id")] is False


def test_ensure_user_tables_upgrades_legacy_nullable_column(scratch_engine):
    with scratch_engine.begin() as conn:
        conn.execute(text("CREATE TABLE app_users (user_id SERIAL PRIMARY KEY, name TEXT, email TEXT, password_hash TEXT)"))
        conn.execute(text("CREATE TABLE user_purchases (id SERIAL PRIMARY KEY, user_id INTEGER, product_id TEXT)"))
    UserRepository(engine=scratch_engine).ensure_user_tables()
    assert _nullable(scratch_engine)[("user_purchases", "user_id")] is False


def test_legacy_rows_without_user_keep_table_usable(scratch_engine):
    with scratch_engine.begin() as conn:
        conn.execute(text("CREATE TABLE app_users (user_id SERIAL PRIMARY KEY, name TEXT, email TEXT, password_hash TEXT)"))
        conn.execute(text("CREATE TABLE user_purchases (id SERIAL PRIMARY KEY, user_id INTEGER, product_id TEXT)"))
        conn.execute(text("INSERT INTO user_purchases (user_id, product_id) VALUES (NULL, 'P1')"))
    UserRepository(engine=scratch_engine).ensure_user_tables()  # must warn, not raise
    with scratch_engine.connect() as conn:
        assert conn.execute(text("SELECT COUNT(*) FROM user_purchases")).scalar() == 1
