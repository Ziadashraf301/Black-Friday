"""Align databases that drifted from init_schema.sql with it and with the ORM (round 2, CORE-16).

Seen on the live warehouse (2026-10-12):
- product_network_metrics was created by pandas: TEXT / BIGINT / DOUBLE columns, no primary key, and the
  JSON lists stored as TEXT, CSV-quoted ('"[{""product_id"": ...}]"') by the old text-format COPY.
- curated_products.sizes / apriori_bundles / item2vec_similars were JSON instead of JSONB (ORM create_all).
- black_friday_cleaned.is_outlier / normalized_purchase / split allowed NULL; idx_cleaned_split was missing.
- user_purchases.user_id allowed NULL (older DDL).

Every step checks the current shape first, so on a database built by 0001 this does nothing.

Revision ID: 0002
Revises: 0001
Create Date: 2026-10-12
"""
from alembic import op
from sqlalchemy import text

revision = "0002"
down_revision = "0001"
branch_labels = None
depends_on = None

# Old rows hold the JSON text wrapped in quotes with doubled inner quotes; unwrap those, cast the rest.
_JSON_FROM_TEXT = (
    "CASE WHEN {col} IS NULL OR btrim({col}) = '' THEN NULL "
    "WHEN left({col}, 1) = '\"' THEN replace(substr({col}, 2, length({col}) - 2), '\"\"', '\"')::jsonb "
    "ELSE {col}::jsonb END"
)


def _columns(conn, table: str) -> dict:
    rows = conn.execute(
        text(
            "SELECT column_name, data_type, is_nullable FROM information_schema.columns "
            "WHERE table_schema = current_schema() AND table_name = :t"
        ),
        {"t": table},
    )
    return {name: (data_type, nullable == "YES") for name, data_type, nullable in rows}


def _has_primary_key(conn, table: str) -> bool:
    return bool(
        conn.execute(
            text(
                "SELECT 1 FROM information_schema.table_constraints "
                "WHERE table_schema = current_schema() AND table_name = :t AND constraint_type = 'PRIMARY KEY'"
            ),
            {"t": table},
        ).first()
    )


def _set_type(conn, table: str, column: str, sql_type: str, data_type: str, using: str = None) -> None:
    current = _columns(conn, table).get(column)
    if current is None or current[0] == data_type:
        return
    using = using or f"{column}::{sql_type}"
    op.execute(f"ALTER TABLE {table} ALTER COLUMN {column} TYPE {sql_type} USING {using}")


def _set_not_null(conn, table: str, column: str) -> None:
    current = _columns(conn, table).get(column)
    if current is None or not current[1]:
        return
    nulls = conn.execute(text(f"SELECT COUNT(*) FROM {table} WHERE {column} IS NULL")).scalar()
    if nulls:
        raise RuntimeError(f"{table}.{column} has {nulls} NULL rows; fix or delete them, then rerun the migration.")
    op.execute(f"ALTER TABLE {table} ALTER COLUMN {column} SET NOT NULL")


def _align_product_network_metrics(conn) -> None:
    table = "product_network_metrics"
    if not _columns(conn, table):
        return
    _set_type(conn, table, "product_id", "VARCHAR(32)", "character varying")
    _set_type(conn, table, "order_count", "INTEGER", "integer")
    for col in ("pagerank_score", "hub_score", "authority_score"):
        _set_type(conn, table, col, "NUMERIC(8, 5)", "numeric")
    _set_type(conn, table, "top_associated_product", "VARCHAR(32)", "character varying")
    _set_type(conn, table, "highest_lift_rule", "NUMERIC(8, 4)", "numeric")
    for col in ("top_bundle_recommendations", "item2vec_recommendations"):
        _set_type(conn, table, col, "JSONB", "jsonb", using=_JSON_FROM_TEXT.format(col=col))
    for col in ("order_count", "pagerank_score", "hub_score", "authority_score"):
        _set_not_null(conn, table, col)
    if not _has_primary_key(conn, table):
        op.execute(f"ALTER TABLE {table} ADD PRIMARY KEY (product_id)")
    op.execute(f"ALTER TABLE {table} ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP")
    op.execute(f"CREATE INDEX IF NOT EXISTS idx_prod_pagerank ON {table} (pagerank_score DESC)")
    op.execute(f"CREATE INDEX IF NOT EXISTS idx_prod_hub ON {table} (hub_score DESC)")
    op.execute(f"CREATE INDEX IF NOT EXISTS idx_prod_authority ON {table} (authority_score DESC)")


def upgrade() -> None:
    conn = op.get_bind()
    _align_product_network_metrics(conn)

    for col in ("sizes", "apriori_bundles", "item2vec_similars"):
        _set_type(conn, "curated_products", col, "JSONB", "jsonb")

    for col in ("is_outlier", "normalized_purchase", "split"):
        _set_not_null(conn, "black_friday_cleaned", col)
    op.execute("CREATE INDEX IF NOT EXISTS idx_cleaned_split ON black_friday_cleaned (split)")

    _set_not_null(conn, "user_purchases", "user_id")
    op.execute("CREATE INDEX IF NOT EXISTS idx_users_email ON app_users (email)")


def downgrade() -> None:
    # The old shapes were accidents (no primary key, quoted JSON text); there is nothing worth restoring.
    pass
