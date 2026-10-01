"""
Warehouse repository for transaction ingestion and cleaned data warehouse queries.
"""
import pandas as pd
from typing import Dict, Optional
from sqlalchemy import text
from core.db.repositories.base import BaseRepository
from core.logging import get_logger

logger = get_logger(__name__)


class WarehouseRepository(BaseRepository):
    """Repository managing raw_black_friday and black_friday_cleaned tables."""

    def truncate_raw_table(self):
        self.truncate_table("raw_black_friday", restart_identity=True)

    def insert_raw_batch(self, df: pd.DataFrame, chunksize: int = 25000):
        self.insert_dataframe("raw_black_friday", df, if_exists="append", chunksize=chunksize)

    def truncate_cleaned_table(self):
        self.truncate_table("black_friday_cleaned", restart_identity=True)

    def insert_cleaned_batch(self, df: pd.DataFrame, chunksize: int = 25000):
        self.insert_dataframe("black_friday_cleaned", df, if_exists="append", chunksize=chunksize)

    def get_raw_records_df(self, limit: Optional[int] = None) -> pd.DataFrame:
        """Retrieves raw ingested records."""
        if limit:
            query = text("SELECT * FROM raw_black_friday LIMIT :limit")
            params = {"limit": int(limit)}
        else:
            query = text("SELECT * FROM raw_black_friday")
            params = {}
        with self.engine.connect() as conn:
            return pd.read_sql(query, conn, params=params)

    def get_cleaned_records_df(
        self,
        exclude_outliers: bool = False,
        split: Optional[str] = None,
        limit: Optional[int] = None
    ) -> pd.DataFrame:
        """Retrieves cleaned and imputed records."""
        query = "SELECT user_id, product_id, gender, age, occupation, city_category, " \
                "stay_in_current_city_years, marital_status, product_category_1, " \
                "product_category_2, product_category_3, purchase, is_outlier, normalized_purchase, split " \
                "FROM black_friday_cleaned"
        conditions = []
        params = {}
        if exclude_outliers:
            conditions.append("is_outlier = FALSE")
        if split:
            conditions.append("split = :split")
            params["split"] = split
        if conditions:
            query += " WHERE " + " AND ".join(conditions)
        if limit:
            query += f" LIMIT {int(limit)}"
        with self.engine.connect() as conn:
            return pd.read_sql(text(query), conn, params=params)

    def get_product_order_counts(self) -> Dict[str, int]:
        """Returns total order count for each product from cleaned transactions."""
        query = text("SELECT product_id, COUNT(*) as order_count FROM black_friday_cleaned GROUP BY product_id")
        with self.engine.connect() as conn:
            return {row["product_id"]: int(row["order_count"]) for row in conn.execute(query).mappings().all()}

    def enable_pgvector_extension(self):
        """Enables pgvector extension and ensures vector and full-text indexes exist."""
        with self.engine.begin() as conn:
            if conn.dialect.name == "postgresql":
                conn.execute(text("CREATE EXTENSION IF NOT EXISTS vector;"))
                conn.execute(text("""
                    CREATE INDEX IF NOT EXISTS idx_curated_embedding_hnsw 
                    ON curated_products USING hnsw (embedding vector_cosine_ops);
                """))
                conn.execute(text("""
                    CREATE INDEX IF NOT EXISTS idx_curated_search_vector 
                    ON curated_products USING gin (search_vector);
                """))
                logger.info("pgvector extension and hybrid indexes verified in PostgreSQL.")


    @staticmethod
    def _map_curated_attributes(p: dict) -> dict:
        """Maps raw product dictionary into typed model attributes."""
        return {
            "name": p.get("name") or p.get("title") or f"Product {p['product_id']}",
            "title": p.get("title") or p.get("name") or f"Product {p['product_id']}",
            "tagline": p.get("tagline"),
            "description": p.get("description"),
            "category_name": p.get("category_name") or p.get("category") or "General",
            "category": p.get("category") or p.get("category_name") or "General",
            "gender": p.get("gender"),
            "brand": p.get("brand"),
            "style": p.get("style"),
            "season": p.get("season"),
            "badge": p.get("badge"),
            "is_hero": bool(p.get("is_hero", False)),
            "sizes": p.get("sizes") or [],
            "rating": float(p.get("rating", 4.5)),
            "review_count": int(p.get("review_count", 100)),
            "original_price": float(p.get("original_price", 99.99)),
            "discounted_price": float(p.get("discounted_price", 49.90)),
            "image_url": p.get("image_url"),
            "order_count": int(p.get("order_count", 0)),
            "product_category_1": int(p.get("product_category_1", 1)),
            "product_category_2": int(p["product_category_2"]) if p.get("product_category_2") is not None else None,
            "product_category_3": int(p["product_category_3"]) if p.get("product_category_3") is not None else None,
            "apriori_bundles": p.get("apriori_bundles"),
            "item2vec_similars": p.get("item2vec_similars"),
        }

    def seed_curated_products(self, products: list):
        """Seeds curated products into curated_products table preserving existing vector embeddings."""
        from core.db.models import Base
        from core.db.models.warehouse import CuratedProduct
        from core.db.session import get_db_session

        self.enable_pgvector_extension()
        Base.metadata.create_all(bind=self.engine)

        with get_db_session() as session:
            for p in products:
                attrs = self._map_curated_attributes(p)
                existing = session.query(CuratedProduct).filter_by(product_id=p["product_id"]).first()
                if existing:
                    for k, v in attrs.items():
                        setattr(existing, k, v)
                else:
                    session.add(CuratedProduct(product_id=p["product_id"], **attrs))
            logger.info(f"Seeded {len(products)} curated catalog products into database.")

    def get_curated_products(self) -> list:
        """Retrieves curated products from curated_products table."""
        from core.db.models import Base
        from core.db.models.warehouse import CuratedProduct
        from core.db.session import get_db_session
        Base.metadata.create_all(bind=self.engine)
        with get_db_session() as session:
            records = session.query(CuratedProduct).all()
            return [
                {
                    "product_id": r.product_id,
                    "name": r.name,
                    "title": r.title,
                    "tagline": r.tagline,
                    "description": r.description,
                    "category_name": r.category_name,
                    "category": r.category,
                    "gender": r.gender,
                    "brand": r.brand,
                    "style": r.style,
                    "season": r.season,
                    "badge": r.badge,
                    "is_hero": r.is_hero,
                    "sizes": r.sizes or [],
                    "rating": float(r.rating) if r.rating is not None else 4.5,
                    "review_count": int(r.review_count) if r.review_count is not None else 100,
                    "original_price": float(r.original_price) if r.original_price is not None else 99.9,
                    "discounted_price": float(r.discounted_price) if r.discounted_price is not None else 49.9,
                    "image_url": r.image_url,
                    "order_count": int(r.order_count) if r.order_count is not None else 0,
                    "product_category_1": int(r.product_category_1) if r.product_category_1 is not None else 1,
                    "product_category_2": int(r.product_category_2) if r.product_category_2 is not None else None,
                    "product_category_3": int(r.product_category_3) if r.product_category_3 is not None else None,
                    "apriori_bundles": r.apriori_bundles or [],
                    "item2vec_similars": r.item2vec_similars or [],
                    "has_embedding": r.embedding is not None,
                }
                for r in records
            ]

    def get_products_missing_embeddings(self) -> list:
        """Retrieves curated products where embedding is NULL (incremental skip logic)."""
        from core.db.models.warehouse import CuratedProduct
        from core.db.session import get_db_session
        with get_db_session() as session:
            records = session.query(CuratedProduct).filter(CuratedProduct.embedding.is_(None)).all()
            return [
                {
                    "product_id": r.product_id,
                    "name": r.name,
                    "tagline": r.tagline,
                    "description": r.description,
                    "category_name": r.category_name,
                    "brand": r.brand,
                    "style": r.style,
                }
                for r in records
            ]

    def update_product_embedding(self, product_id: str, embedding: list, search_text: str = None) -> None:
        """Updates embedding vector and tsvector search text for a product."""
        from sqlalchemy import text
        from core.db.session import get_db_session
        with get_db_session() as session:
            stmt = text("""
                UPDATE curated_products
                SET embedding = :emb,
                    search_vector = to_tsvector('english', :stext)
                WHERE product_id = :pid
            """)
            session.execute(stmt, {
                "emb": str(embedding),
                "stext": search_text or "",
                "pid": product_id,
            })
            session.commit()