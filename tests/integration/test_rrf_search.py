"""CORE-5: RRF candidate CTEs must keep the best-ranked rows, and the size filter must be valid JSON."""

import json

import pytest
from sqlalchemy import text

from core.db.repository import BlackFridayRepository


@pytest.fixture(scope="module")
def repo():
    repo = BlackFridayRepository()
    try:
        with repo.engine.connect() as conn:
            n = conn.execute(text("SELECT COUNT(*) FROM curated_products WHERE embedding IS NOT NULL")).scalar()
    except Exception as exc:
        pytest.skip(f"PostgreSQL unavailable: {exc}")
    if n < 25:
        pytest.skip("needs more than 20 embedded curated products to exercise the candidate LIMIT")
    return repo


def test_nearest_vector_survives_candidate_limit(repo):
    with repo.engine.connect() as conn:
        rows = conn.execute(
            text("SELECT product_id, embedding::text FROM curated_products WHERE embedding IS NOT NULL")
        ).fetchall()
    for pid, emb in rows[:5]:
        results = repo.warehouse._execute_rrf_query(query_vec_str=emb, query_text="zzzzqq", top_k=1)
        assert results and results[0]["product_id"] == pid


def test_size_with_quote_is_valid_json_filter(repo):
    # A raw f-string would build '["M"X"]' and make the jsonb cast fail.
    assert (
        repo.warehouse._execute_rrf_query(
            query_vec_str="[" + ",".join(["0.1"] * 768) + "]", query_text="x", size='M"X', top_k=1
        )
        == []
    )
    assert json.dumps(['M"X']) == '["M\\"X"]'
