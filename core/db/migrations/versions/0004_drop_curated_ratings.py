"""Drop curated_products.rating and review_count (round 2, step 6.5).

The Black Friday dataset has no ratings or reviews: the values were invented (30 of 50 products in
data/curated_products.json, 4.5 / 100 database defaults for the rest), and neither the API nor the UI shows them.

Revision ID: 0004
Revises: 0003
Create Date: 2026-10-12
"""

from alembic import op

revision = "0004"
down_revision = "0003"
branch_labels = None
depends_on = None


def upgrade() -> None:
    op.execute("ALTER TABLE curated_products DROP COLUMN IF EXISTS rating")
    op.execute("ALTER TABLE curated_products DROP COLUMN IF EXISTS review_count")


def downgrade() -> None:
    # The old values were invented, so they are not restored; the columns come back with their old defaults.
    op.execute("ALTER TABLE curated_products ADD COLUMN IF NOT EXISTS rating FLOAT NOT NULL DEFAULT 4.5")
    op.execute("ALTER TABLE curated_products ADD COLUMN IF NOT EXISTS review_count INTEGER NOT NULL DEFAULT 100")
