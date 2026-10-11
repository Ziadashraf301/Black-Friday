"""CORE-4: bulk COPY must round-trip values containing tabs, quotes, newlines and backslashes."""

import json

import pandas as pd
import pytest
from sqlalchemy import text

from core.db.repositories.base import BaseRepository

TABLE = "tmp_core4_copy"


@pytest.fixture
def repo():
    repo = BaseRepository()
    try:
        with repo.engine.begin() as conn:
            conn.execute(text(f"DROP TABLE IF EXISTS {TABLE}"))
            conn.execute(text(f"CREATE TABLE {TABLE} (id INTEGER, label TEXT, payload TEXT)"))
    except Exception as exc:
        pytest.skip(f"PostgreSQL unavailable: {exc}")
    repo.VALID_TABLES = repo.VALID_TABLES | {TABLE}
    yield repo
    with repo.engine.begin() as conn:
        conn.execute(text(f"DROP TABLE IF EXISTS {TABLE}"))


def test_special_characters_round_trip_via_copy(repo, caplog):
    labels = ["plain", 'has "quotes"', "tab\there", "line\nbreak", "back\\slash", None]
    df = pd.DataFrame(
        {
            "id": range(len(labels)),
            "label": labels,
            "payload": [json.dumps(["P1", "P2"])] * len(labels),
        }
    )
    with caplog.at_level("WARNING"):
        repo.insert_dataframe(TABLE, df, if_exists="replace")
    assert "Fast COPY failed" not in caplog.text

    with repo.engine.connect() as conn:
        rows = conn.execute(text(f"SELECT label, payload FROM {TABLE} ORDER BY id")).fetchall()
    assert [r[0] for r in rows] == labels
    assert all(json.loads(r[1]) == ["P1", "P2"] for r in rows)
