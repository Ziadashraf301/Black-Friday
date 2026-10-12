"""CORE-13: init_schema.sql must never be looser than the ORM models on NOT NULL.

The old runtime DDL (`ensure_user_tables`) is gone: Alembic owns the schema, and migration 0002 repairs a legacy
nullable `user_purchases.user_id` (tests/integration/test_migrations.py)."""

from pathlib import Path

import pytest
from sqlalchemy import create_engine, text

from core.config import settings
from core.db.models import Base

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
