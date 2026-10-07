"""
Regression test for Fix 3.2:
- index=True on product_id of black_friday_cleaned
- Verify generated DDL / pg_indexes
"""
from sqlalchemy import inspect, text
from core.db.session import get_db_engine
from core.db.models.warehouse import BlackFridayCleaned


def test_black_friday_cleaned_product_id_has_index():
    """Verify ORM model definition specifies index=True on product_id."""
    col = BlackFridayCleaned.__table__.c.product_id
    assert col.index is True or any(
        "product_id" in idx.columns.keys() for idx in BlackFridayCleaned.__table__.indexes
    ), "product_id column does not have index=True in BlackFridayCleaned model"


def test_database_has_index_on_product_id():
    """Verify PostgreSQL database has an index covering product_id in black_friday_cleaned."""
    engine = get_db_engine()
    inspector = inspect(engine)
    indexes = inspector.get_indexes("black_friday_cleaned")

    indexed_cols = []
    for idx in indexes:
        indexed_cols.extend(idx.get("column_names", []))

    assert "product_id" in indexed_cols, (
        f"product_id not found in indexes of black_friday_cleaned: {indexes}"
    )

    # Also verify via pg_indexes query
    with engine.connect() as conn:
        result = conn.execute(text("""
            SELECT indexname, indexdef 
            FROM pg_indexes 
            WHERE tablename = 'black_friday_cleaned' AND indexdef ILIKE '%product_id%';
        """)).fetchall()
        assert len(result) > 0, "No index found in pg_indexes covering product_id"
