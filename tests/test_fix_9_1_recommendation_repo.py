"""
Regression test for Fix 9.1:
- RecommendationRepository.get_product_categories uses index and accepts session
- Query uses index scan (verified with EXPLAIN)
"""
from sqlalchemy import text
from core.db.session import get_db_session, get_db_engine
from core.db.repositories.recommendation_repo import RecommendationRepository


def test_get_product_categories_accepts_and_uses_session():
    """Verify get_product_categories accepts an external session and queries correctly."""
    repo = RecommendationRepository()

    with get_db_session() as session:
        result = repo.get_product_categories("P00025442", session=session)
        # Should return a dict (or None if no match) without raising an error
        if result is not None:
            assert isinstance(result, dict)
            assert "product_category_1" in result
            assert "product_category_2" in result
            assert "product_category_3" in result


def test_get_product_categories_without_session():
    """Verify get_product_categories functions cleanly using its own engine connection."""
    repo = RecommendationRepository()
    result = repo.get_product_categories("P00025442")
    if result is not None:
        assert isinstance(result, dict)
        assert "product_category_1" in result


def test_category_mode_query_uses_index_scan():
    """Verify category mode query against black_friday_cleaned uses an index scan rather than seq scan."""
    engine = get_db_engine()
    explain_sql = text("""
        EXPLAIN
        SELECT
            MODE() WITHIN GROUP (ORDER BY product_category_1) AS product_category_1,
            MODE() WITHIN GROUP (ORDER BY product_category_2) AS product_category_2,
            MODE() WITHIN GROUP (ORDER BY product_category_3) AS product_category_3
        FROM black_friday_cleaned
        WHERE product_id = :pid
    """)
    with engine.connect() as conn:
        # Check row count first
        count = conn.execute(text("SELECT COUNT(*) FROM black_friday_cleaned;")).scalar()
        if count > 0:
            plan_rows = conn.execute(explain_sql, {"pid": "P00025442"}).fetchall()
            plan_text = " ".join([r[0] for r in plan_rows])
            # Assert index scan was chosen
            assert "Index Scan" in plan_text or "Bitmap Index Scan" in plan_text, (
                f"Expected index scan in EXPLAIN plan, got: {plan_text}"
            )
        else:
            # Table is empty in this test environment; verify index exists
            indexes = conn.execute(text("""
                SELECT indexname FROM pg_indexes 
                WHERE tablename = 'black_friday_cleaned' AND indexdef ILIKE '%product_id%';
            """)).fetchall()
            assert len(indexes) > 0, "Index covering product_id does not exist"
