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


def test_upgrade_repairs_live_drift(scratch_url):
    _run_sql(scratch_url, SCHEMA_SQL.read_text(encoding="utf-8"))
    _run_sql(
        scratch_url,
        f"""
        DROP TABLE product_network_metrics;
        CREATE TABLE product_network_metrics (
            product_id TEXT, order_count BIGINT, pagerank_score DOUBLE PRECISION, hub_score DOUBLE PRECISION,
            authority_score DOUBLE PRECISION, top_associated_product TEXT, highest_lift_rule DOUBLE PRECISION,
            top_bundle_recommendations TEXT, item2vec_recommendations TEXT
        );
        INSERT INTO product_network_metrics VALUES
            ('P00025442', 7, 0.24014, 0.02671, 0.0, 'P00112142', 1.5998, '{QUOTED_BUNDLES.replace("'", "''")}', '[]'),
            ('P00112142', 3, 0.00173, 0.0, 0.01, NULL, NULL, NULL, '["P00025442"]');
        ALTER TABLE curated_products ALTER COLUMN sizes TYPE JSON, ALTER COLUMN apriori_bundles TYPE JSON,
            ALTER COLUMN item2vec_similars TYPE JSON;
        INSERT INTO curated_products (product_id, sizes) VALUES ('P00025442', '["S", "M"]');
        ALTER TABLE black_friday_cleaned ALTER COLUMN is_outlier DROP NOT NULL,
            ALTER COLUMN normalized_purchase DROP NOT NULL, ALTER COLUMN split DROP NOT NULL;
        DROP INDEX idx_cleaned_split;
        ALTER TABLE user_purchases ALTER COLUMN user_id DROP NOT NULL;
        ALTER TABLE semantic_query_cache DROP COLUMN user_key;
        """,
    )
    _upgrade(scratch_url)

    engine = create_engine(scratch_url)
    try:
        insp = inspect(engine)
        assert insp.get_pk_constraint("product_network_metrics")["constrained_columns"] == ["product_id"]
        not_null = {c["name"]: c["nullable"] for c in insp.get_columns("black_friday_cleaned")}
        assert not any(not_null[c] for c in ("is_outlier", "normalized_purchase", "split"))
        assert "user_key" in {c["name"] for c in insp.get_columns("semantic_query_cache")}
        with engine.connect() as conn:
            rows = dict(
                conn.execute(
                    text("SELECT product_id, top_bundle_recommendations FROM product_network_metrics")
                ).fetchall()
            )
            sizes = conn.execute(text("SELECT sizes FROM curated_products")).scalar()
    finally:
        engine.dispose()

    assert rows["P00025442"] == json.loads(QUOTED_BUNDLES[1:-1].replace('""', '"'))
    assert rows["P00112142"] is None
    assert sizes == ["S", "M"]
    assert "idx_cleaned_split" in _indexes(scratch_url)
    assert _schema_diff(scratch_url) == []


def test_upgrade_refuses_to_drop_null_purchase_owners(scratch_url):
    _run_sql(scratch_url, SCHEMA_SQL.read_text(encoding="utf-8"))
    _run_sql(
        scratch_url,
        """
        ALTER TABLE user_purchases ALTER COLUMN user_id DROP NOT NULL;
        INSERT INTO user_purchases (user_id, product_id) VALUES (NULL, 'P1');
        """,
    )
    with pytest.raises(RuntimeError, match="user_purchases.user_id has 1 NULL"):
        _upgrade(scratch_url)
