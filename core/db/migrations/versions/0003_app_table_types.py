"""Application-table types and keys from the ERD observations (docs/DATABASE_ERD.md, section 6).

1. Money is exact: curated_products.original_price / discounted_price and user_purchases.predicted_usd become
   NUMERIC(10, 2) (every stored value already has at most 2 decimals). The invented 99.9 / 49.9 price defaults are dropped.
2. user_carts.updated_at becomes TIMESTAMPTZ (default now()). Old ISO-8601 text converts; anything else becomes NULL.
3. Sizes match the warehouse: app_users.gender VARCHAR(8), city_category VARCHAR(4), user_purchases.product_id VARCHAR(32).
4. user_purchases.product_id references curated_products.product_id (purchases only come from the catalogue). The
   migration refuses to add it while orphan rows exist. Warehouse tables get no foreign keys (bulk TRUNCATE + COPY).

Every step checks the current shape first, so it is safe to rerun.

Revision ID: 0003
Revises: 0002
Create Date: 2026-10-12
"""

from alembic import op
from sqlalchemy import text

revision = "0003"
down_revision = "0002"
branch_labels = None
depends_on = None

PURCHASE_PRODUCT_FK = "fk_user_purchases_product_id"

# Text written by the old code was datetime.isoformat(); anything else (empty, garbage) is dropped, not guessed
_TIMESTAMP_FROM_TEXT = (
    "CASE WHEN updated_at ~ '^\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}' THEN updated_at::timestamptz ELSE NULL END"
)


def _column(conn, table: str, column: str):
    return conn.execute(
        text(
            "SELECT data_type, character_maximum_length, numeric_precision, numeric_scale, column_default "
            "FROM information_schema.columns "
            "WHERE table_schema = current_schema() AND table_name = :t AND column_name = :c"
        ),
        {"t": table, "c": column},
    ).first()


def _set_type(conn, table: str, column: str, sql_type: str, matches, using: str = None) -> None:
    current = _column(conn, table, column)
    if current is None or matches(current):
        return
    op.execute(f"ALTER TABLE {table} ALTER COLUMN {column} TYPE {sql_type} USING {using or f'{column}::{sql_type}'}")


def _money(conn, table: str, column: str) -> None:
    _set_type(conn, table, column, "NUMERIC(10, 2)", lambda c: c[0] == "numeric" and (c[2], c[3]) == (10, 2))


def _varchar(conn, table: str, column: str, length: int) -> None:
    longest = conn.execute(text(f"SELECT COALESCE(MAX(length({column})), 0) FROM {table}")).scalar()
    if longest > length:
        raise RuntimeError(f"{table}.{column} has values longer than {length}; fix them, then rerun the migration.")
    _set_type(conn, table, column, f"VARCHAR({length})", lambda c: c[0] == "character varying" and c[1] == length)


def _has_constraint(conn, name: str) -> bool:
    return bool(conn.execute(text("SELECT 1 FROM pg_constraint WHERE conname = :n"), {"n": name}).first())


def upgrade() -> None:
    conn = op.get_bind()

    # 1. Exact money
    for column in ("original_price", "discounted_price"):
        _money(conn, "curated_products", column)
        op.execute(f"ALTER TABLE curated_products ALTER COLUMN {column} DROP DEFAULT")
    _money(conn, "user_purchases", "predicted_usd")

    # 2. Cart time as a real timestamp
    if _column(conn, "user_carts", "updated_at") is not None:
        _set_type(
            conn,
            "user_carts",
            "updated_at",
            "TIMESTAMP WITH TIME ZONE",
            lambda c: c[0] == "timestamp with time zone",
            using=_TIMESTAMP_FROM_TEXT,
        )
        op.execute("ALTER TABLE user_carts ALTER COLUMN updated_at SET DEFAULT now()")

    # 3. Same sizes as the warehouse
    _varchar(conn, "app_users", "gender", 8)
    _varchar(conn, "app_users", "city_category", 4)
    _varchar(conn, "user_purchases", "product_id", 32)

    # 4. Purchases point at a catalogue product
    if not _has_constraint(conn, PURCHASE_PRODUCT_FK):
        orphans = conn.execute(
            text(
                "SELECT COUNT(*) FROM user_purchases up "
                "WHERE NOT EXISTS (SELECT 1 FROM curated_products cp WHERE cp.product_id = up.product_id)"
            )
        ).scalar()
        if orphans:
            raise RuntimeError(
                f"user_purchases has {orphans} rows whose product_id is not in curated_products; "
                "re-seed the catalogue or remove those rows, then rerun the migration."
            )
        op.execute(
            f"ALTER TABLE user_purchases ADD CONSTRAINT {PURCHASE_PRODUCT_FK} "
            "FOREIGN KEY (product_id) REFERENCES curated_products (product_id)"
        )
    op.execute("CREATE INDEX IF NOT EXISTS idx_purchases_product_id ON user_purchases (product_id)")


def downgrade() -> None:
    op.execute("DROP INDEX IF EXISTS idx_purchases_product_id")
    op.execute(f"ALTER TABLE user_purchases DROP CONSTRAINT IF EXISTS {PURCHASE_PRODUCT_FK}")
    op.execute("ALTER TABLE user_purchases ALTER COLUMN product_id TYPE TEXT")
    op.execute("ALTER TABLE app_users ALTER COLUMN city_category TYPE VARCHAR(1)")  # fails if longer values exist
    op.execute("ALTER TABLE app_users ALTER COLUMN gender TYPE VARCHAR(1)")
    op.execute("ALTER TABLE user_carts ALTER COLUMN updated_at DROP DEFAULT")
    op.execute("ALTER TABLE user_carts ALTER COLUMN updated_at TYPE VARCHAR(64) USING updated_at::text")
    op.execute("ALTER TABLE user_purchases ALTER COLUMN predicted_usd TYPE DOUBLE PRECISION")
    for column, default in (("original_price", "99.9"), ("discounted_price", "49.9")):
        op.execute(f"ALTER TABLE curated_products ALTER COLUMN {column} TYPE DOUBLE PRECISION")
        op.execute(f"ALTER TABLE curated_products ALTER COLUMN {column} SET DEFAULT {default}")
