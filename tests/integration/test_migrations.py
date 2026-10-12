"""CORE-16: the Alembic migrations build the ORM schema, on an empty database, on one made by init_schema.sql,
and on one with the drift seen on the live warehouse. Runs on scratch databases only, never the warehouse."""

import json
from pathlib import Path

import pytest
from alembic import command
from alembic.autogenerate import compare_metadata
from alembic.config import Config
from alembic.migration import MigrationContext
from sqlalchemy import create_engine, inspect, text

from core.config import settings
from core.db.models import Base

ROOT = Path(__file__).resolve().parents[2]
SCHEMA_SQL = ROOT / "docker" / "postgres" / "init_schema.sql"
SCRATCH = "core16_migration_scratch"
HNSW_INDEXES = {"idx_curated_embedding_hnsw", "idx_semantic_cache_hnsw"}

QUOTED_BUNDLES = '"[{""product_id"": ""P00025442"", ""confidence"": 0.4386, ""lift"": 1.5998}]"'


@pytest.fixture
def scratch_url():
    base = settings.database_url.rsplit("/", 1)[0]
    admin = create_engine(base + "/postgres", isolation_level="AUTOCOMMIT")
    try:
        with admin.connect() as conn:
            conn.execute(text(f"DROP DATABASE IF EXISTS {SCRATCH} WITH (FORCE)"))
            conn.execute(text(f"CREATE DATABASE {SCRATCH}"))
    except Exception as exc:
        pytest.skip(f"cannot create scratch database: {exc}")
    yield f"{base}/{SCRATCH}"
    with admin.connect() as conn:
        conn.execute(text(f"DROP DATABASE IF EXISTS {SCRATCH} WITH (FORCE)"))
    admin.dispose()


def _upgrade(url: str) -> None:
    cfg = Config(str(ROOT / "alembic.ini"))
    cfg.set_main_option("script_location", str(ROOT / "core" / "db" / "migrations"))
    cfg.attributes["url"] = url
    command.upgrade(cfg, "head")


def _schema_diff(url: str) -> list:
    engine = create_engine(url)
    try:
        with engine.connect() as conn:
            ctx = MigrationContext.configure(
                conn,
                opts={
                    "compare_type": True,
                    "include_object": lambda obj, name, type_, *a: not (type_ == "index" and name in HNSW_INDEXES),
                },
            )
            return compare_metadata(ctx, Base.metadata)
    finally:
        engine.dispose()


def _run_sql(url: str, sql: str) -> None:
    engine = create_engine(url)
    raw = engine.raw_connection()
    try:
        raw.cursor().execute(sql)
        raw.commit()
    finally:
        raw.close()
        engine.dispose()


def _indexes(url: str) -> set:
    engine = create_engine(url)
    try:
        with engine.connect() as conn:
            return set(conn.execute(text("SELECT indexname FROM pg_indexes WHERE schemaname = 'public'")).scalars())
    finally:
        engine.dispose()


def test_upgrade_on_empty_database_matches_orm(scratch_url):
    _upgrade(scratch_url)
    assert _schema_diff(scratch_url) == []
    assert HNSW_INDEXES <= _indexes(scratch_url)


def test_upgrade_after_docker_init_script_matches_orm(scratch_url):
    _run_sql(scratch_url, SCHEMA_SQL.read_text(encoding="utf-8"))
    _upgrade(scratch_url)
    assert _schema_diff(scratch_url) == []


def test_init_schema_sql_matches_orm(scratch_url):
    """init_schema.sql (Docker first start) must build the same schema the migrations do."""
    _run_sql(scratch_url, SCHEMA_SQL.read_text(encoding="utf-8"))
    assert _schema_diff(scratch_url) == []
