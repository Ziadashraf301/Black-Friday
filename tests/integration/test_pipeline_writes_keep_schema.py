"""CORE-18: pipeline writes (insert_dataframe "replace") keep the migrated schema and never let pandas create
a table. Runs on a scratch database only, never the warehouse."""

import pandas as pd
import pytest
from sqlalchemy import create_engine, inspect, text

from core.config import settings
from core.db.repositories.recommendation_repo import RecommendationRepository

SCRATCH = "core18_pipeline_scratch"


@pytest.fixture
def scratch_engine():
    base = settings.database_url.rsplit("/", 1)[0]
    admin = create_engine(base + "/postgres", isolation_level="AUTOCOMMIT")
    try:
        with admin.connect() as conn:
            conn.execute(text(f"DROP DATABASE IF EXISTS {SCRATCH} WITH (FORCE)"))
            conn.execute(text(f"CREATE DATABASE {SCRATCH}"))
    except Exception as exc:
        pytest.skip(f"cannot create scratch database: {exc}")
    engine = create_engine(f"{base}/{SCRATCH}")
    yield engine
    engine.dispose()
    with admin.connect() as conn:
        conn.execute(text(f"DROP DATABASE IF EXISTS {SCRATCH} WITH (FORCE)"))
    admin.dispose()


def _metrics(product_ids):
    return pd.DataFrame(
        [
            {
                "product_id": pid,
                "order_count": 5,
                "pagerank_score": 0.12345,
                "hub_score": 0.01,
                "authority_score": 0.02,
                "top_associated_product": "P00000001",
                "highest_lift_rule": 1.5,
                "top_bundle_recommendations": [{"product_id": "P00000001", "lift": 1.5}],
                "item2vec_recommendations": ["P00000002"],
            }
            for pid in product_ids
        ]
    )


def test_replace_keeps_primary_key_and_jsonb(scratch_engine):
    repo = RecommendationRepository(engine=scratch_engine)
    repo.create_app_tables()
    repo.insert_product_network_metrics(_metrics(["P1", "P2"]))
    repo.insert_product_network_metrics(_metrics(["P3"]))  # second run replaces the rows, not the table

    insp = inspect(scratch_engine)
    assert insp.get_pk_constraint("product_network_metrics")["constrained_columns"] == ["product_id"]
    with scratch_engine.connect() as conn:
        types = dict(
            conn.execute(
                text(
                    "SELECT column_name, data_type FROM information_schema.columns "
                    "WHERE table_name = 'product_network_metrics'"
                )
            ).fetchall()
        )
        rows = conn.execute(text("SELECT product_id, top_bundle_recommendations FROM product_network_metrics")).fetchall()
    assert types["top_bundle_recommendations"] == "jsonb"
    assert types["pagerank_score"] == "numeric"
    assert rows == [("P3", [{"product_id": "P00000001", "lift": 1.5}])]


def test_missing_table_is_not_created_by_pandas(scratch_engine):
    repo = RecommendationRepository(engine=scratch_engine)
    with pytest.raises(RuntimeError, match="make migrate"):
        repo.insert_product_network_metrics(_metrics(["P1"]))
    assert not inspect(scratch_engine).has_table("product_network_metrics")
