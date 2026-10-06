"""
Warehouse repository for transaction ingestion and cleaned data warehouse queries.
"""
import pandas as pd
from typing import Dict, Optional, List, Any
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

    def _execute_rrf_query(
        self,
        query_vec_str: str,
        query_text: str,
        target_categories: Optional[List[str]] = None,
        max_price: Optional[float] = None,
        size: Optional[str] = None,
        top_k: int = 5,
    ) -> List[Dict[str, Any]]:
        """Executes a single RRF hybrid vector + keyword query with specified SQL filters."""
        where_clauses = ["1=1"]
        params: Dict[str, Any] = {
            "query_vec": query_vec_str,
            "query_text": query_text,
            "top_k_candidates": max(top_k * 3, 20),
        }

        if max_price is not None:
            where_clauses.append("discounted_price <= :max_price")
            params["max_price"] = max_price

        if target_categories:
            where_clauses.append("category_name = ANY(:target_categories)")
            params["target_categories"] = target_categories

        if size is not None:
            where_clauses.append("sizes::jsonb @> CAST(:size_json AS jsonb)")
            params["size_json"] = f'["{size.strip().upper()}"]'

        filter_sql = " AND ".join(where_clauses)

        rrf_query = text(f"""
            WITH dense_ranks AS (
                SELECT 
                    product_id,
                    ROW_NUMBER() OVER (ORDER BY embedding <=> CAST(:query_vec AS vector)) AS dense_rank
                FROM curated_products
                WHERE {filter_sql}
                LIMIT :top_k_candidates
            ),
            sparse_ranks AS (
                SELECT 
                    product_id,
                    ROW_NUMBER() OVER (ORDER BY ts_rank_cd(search_vector, plainto_tsquery('english', :query_text)) DESC) AS sparse_rank
                FROM curated_products
                WHERE {filter_sql}
                LIMIT :top_k_candidates
            ),
            combined_scores AS (
                SELECT 
                    COALESCE(d.product_id, s.product_id) AS product_id,
                    (COALESCE(1.0 / (60.0 + d.dense_rank), 0.0) + COALESCE(1.0 / (60.0 + s.sparse_rank), 0.0)) AS rrf_score
                FROM dense_ranks d
                FULL OUTER JOIN sparse_ranks s ON d.product_id = s.product_id
            )
            SELECT 
                p.product_id,
                p.name,
                p.category_name,
                p.original_price,
                p.discounted_price,
                p.badge,
                p.sizes,
                p.image_url,
                p.style,
                c.rrf_score
            FROM combined_scores c
            JOIN curated_products p ON c.product_id = p.product_id
            ORDER BY c.rrf_score DESC
            LIMIT {top_k};
        """)

        with self.engine.connect() as conn:
            rows = conn.execute(rrf_query, params).fetchall()

        results: List[Dict[str, Any]] = []
        for r in rows:
            results.append({
                "product_id": r[0],
                "name": r[1],
                "category_name": r[2],
                "original_price": float(r[3]),
                "discounted_price": float(r[4]),
                "badge": r[5],
                "sizes": list(r[6]) if r[6] else [],
                "image_url": r[7] or f"/products/{r[0]}.jpg",
                "style": r[8],
                "score": round(float(r[9]), 4),
            })
        return results

    def hybrid_search_products(
        self,
        query_vec: List[float],
        query_text: str,
        target_categories: Optional[List[str]] = None,
        max_price: Optional[float] = None,
        size: Optional[str] = None,
        top_k: int = 5,
    ) -> tuple[List[Dict[str, Any]], str]:
        """
        Progressive Search Relaxation Ladder (Phase 4 - Task P4-04).
        Executes 4-tier relaxation:
          - Tier 1: TIER_1_STRICT -> Full constraints (category + size + budget + hybrid RRF)
          - Tier 2: TIER_2_RELAX_SIZE -> Relaxes size constraint if 0 hits
          - Tier 3: TIER_3_RELAX_BUDGET -> Expands budget by +25% if 0 hits
          - Tier 4: TIER_4_ZERO_DATA_DEALS -> Guaranteed doorbuster discount fallback
        """
        vec_str = "[" + ",".join(str(x) for x in query_vec) + "]"

        # Tier 1: Strict search
        results = self._execute_rrf_query(
            query_vec_str=vec_str,
            query_text=query_text,
            target_categories=target_categories,
            max_price=max_price,
            size=size,
            top_k=top_k,
        )
        if results:
            for r in results:
                r["relaxation_level"] = "TIER_1_STRICT"
            return results, "TIER_1_STRICT"

        # Tier 2: Relax size
        if size is not None:
            logger.info(f"[SEARCH: LADDER] Tier 1 yielded 0 results. Relaxing size='{size}' -> Tier 2")
            results = self._execute_rrf_query(
                query_vec_str=vec_str,
                query_text=query_text,
                target_categories=target_categories,
                max_price=max_price,
                size=None,
                top_k=top_k,
            )
            if results:
                for r in results:
                    r["relaxation_level"] = "TIER_2_RELAX_SIZE"
                return results, "TIER_2_RELAX_SIZE"

        # Tier 3: Relax budget (+25%)
        if max_price is not None:
            expanded_budget = round(max_price * 1.25, 2)
            logger.info(f"[SEARCH: LADDER] Tier 2 yielded 0 results. Expanding budget ${max_price:.2f} -> ${expanded_budget:.2f} (+25%) -> Tier 3")
            results = self._execute_rrf_query(
                query_vec_str=vec_str,
                query_text=query_text,
                target_categories=target_categories,
                max_price=expanded_budget,
                size=None,
                top_k=top_k,
            )
            if results:
                for r in results:
                    r["relaxation_level"] = "TIER_3_RELAX_BUDGET"
                return results, "TIER_3_RELAX_BUDGET"

        # Tier 4: Guaranteed Doorbuster Deals Fallback
        logger.info("[SEARCH: LADDER] All constraints yielded 0 results. Activating Tier 4 Zero-Data Deals fallback.")
        fallback_query = text(f"""
            SELECT 
                p.product_id,
                p.name,
                p.category_name,
                p.original_price,
                p.discounted_price,
                p.badge,
                p.sizes,
                p.image_url,
                p.style,
                0.50 AS score
            FROM curated_products p
            WHERE p.is_hero = TRUE OR p.badge ILIKE '%deal%' OR p.badge ILIKE '%sale%'
            ORDER BY ((p.original_price - p.discounted_price) / NULLIF(p.original_price, 0)) DESC
            LIMIT {top_k};
        """)
        with self.engine.connect() as conn:
            rows = conn.execute(fallback_query).fetchall()

        deals: List[Dict[str, Any]] = []
        for r in rows:
            deals.append({
                "product_id": r[0],
                "name": r[1],
                "category_name": r[2],
                "original_price": float(r[3]),
                "discounted_price": float(r[4]),
                "badge": r[5] or "Black Friday Steal",
                "sizes": list(r[6]) if r[6] else ["S", "M", "L"],
                "image_url": r[7] or f"/products/{r[0]}.jpg",
                "style": r[8],
                "score": 0.50,
                "relaxation_level": "TIER_4_ZERO_DATA_DEALS",
            })
        return deals, "TIER_4_ZERO_DATA_DEALS"

    def get_curated_product_by_id(self, product_id: str) -> Optional[Dict[str, Any]]:
        """Retrieves a single curated product specification from curated_products table."""
        query = text("""
            SELECT 
                product_id, name, tagline, description, category_name,
                sizes, original_price, discounted_price, badge, image_url,
                gender, brand, style, season
            FROM curated_products
            WHERE product_id = :pid
            LIMIT 1;
        """)
        with self.engine.connect() as conn:
            row = conn.execute(query, {"pid": product_id}).fetchone()
            if not row:
                return None
            return {
                "product_id": row[0],
                "name": row[1],
                "tagline": row[2] or "",
                "description": row[3] or "",
                "category_name": row[4],
                "sizes": list(row[5]) if row[5] else ["S", "M", "L"],
                "original_price": float(row[6]),
                "discounted_price": float(row[7]),
                "badge": row[8],
                "image_url": row[9] or f"/products/{row[0]}.jpg",
                "gender": row[10] or "Unisex",
                "brand": row[11] or "Heritage Guild",
                "style": row[12] or "Classic",
                "season": row[13] or "All-Season",
            }

    def get_product_bundles(self, product_id: str) -> Optional[Dict[str, Any]]:
        """Retrieves raw apriori_bundles and item2vec_similars from curated_products table."""
        query = text("""
            SELECT product_id, name, discounted_price, apriori_bundles, item2vec_similars
            FROM curated_products
            WHERE product_id = :pid
            LIMIT 1;
        """)
        with self.engine.connect() as conn:
            row = conn.execute(query, {"pid": product_id}).fetchone()
            if not row:
                return None
            return {
                "product_id": row[0],
                "name": row[1],
                "discounted_price": float(row[2]),
                "apriori_bundles": row[3] or [],
                "item2vec_similars": row[4] or [],
            }

    def ensure_user_carts_table(self):
        """Ensures user_carts cold-tier table exists."""
        with self.engine.begin() as conn:
            conn.execute(text("""
                CREATE TABLE IF NOT EXISTS user_carts (
                    user_id VARCHAR(64) PRIMARY KEY,
                    session_id VARCHAR(64) NOT NULL,
                    cart_data JSONB NOT NULL,
                    item_count INT DEFAULT 0,
                    total_amount NUMERIC(10, 2) DEFAULT 0.00,
                    updated_at VARCHAR(64)
                );
            """))

    def save_user_cart_snapshot(self, user_id: str, session_id: str, cart_dict: Dict[str, Any]) -> bool:
        """Upserts cart snapshot to PostgreSQL user_carts table."""
        self.ensure_user_carts_table()
        import json
        from datetime import datetime, timezone
        query = text("""
            INSERT INTO user_carts (user_id, session_id, cart_data, item_count, total_amount, updated_at)
            VALUES (:uid, :sid, CAST(:cdata AS jsonb), :icount, :tot, :upd)
            ON CONFLICT (user_id) DO UPDATE SET
                session_id = EXCLUDED.session_id,
                cart_data = EXCLUDED.cart_data,
                item_count = EXCLUDED.item_count,
                total_amount = EXCLUDED.total_amount,
                updated_at = EXCLUDED.updated_at;
        """)
        with self.engine.begin() as conn:
            conn.execute(query, {
                "uid": user_id,
                "sid": session_id,
                "cdata": json.dumps(cart_dict),
                "icount": int(cart_dict.get("item_count", 0)),
                "tot": float(cart_dict.get("final_total", 0.0)),
                "upd": datetime.now(timezone.utc).isoformat(),
            })
        return True

    def load_user_cart_snapshot(self, user_id: str) -> Optional[Dict[str, Any]]:
        """Retrieves cold-tier cart snapshot from PostgreSQL."""
        self.ensure_user_carts_table()
        import json
        query = text("""
            SELECT cart_data FROM user_carts WHERE user_id = :uid LIMIT 1;
        """)
        with self.engine.connect() as conn:
            row = conn.execute(query, {"uid": user_id}).fetchone()
            if row and row[0]:
                return row[0] if isinstance(row[0], dict) else json.loads(row[0])
        return None

    def check_product_stock(self, product_id: str, size: Optional[str] = None, quantity: int = 1) -> bool:
        """Validates inventory and catalog sizing availability for a product."""
        clean_id = product_id.strip().upper()
        query = text("""
            SELECT sizes FROM curated_products WHERE product_id = :pid LIMIT 1;
        """)
        with self.engine.connect() as conn:
            row = conn.execute(query, {"pid": clean_id}).fetchone()
            if not row:
                return False
            sizes = row[0] or []
            if size:
                return size.upper() in [s.upper() for s in sizes]
            return True

    def find_semantic_cached_response(
        self,
        query_vec: List[float],
        max_distance: float = 0.08,  # Cosine similarity >= 0.92
    ) -> Optional[Dict[str, Any]]:
        """
        True Vector Semantic Cache lookup (pgvector HNSW index).
        Computes cosine distance <=> between incoming query_vec and cached queries.
        Returns cached response if distance <= max_distance (similarity >= 0.92).
        """
        vec_str = "[" + ",".join(str(x) for x in query_vec) + "]"
        query = text("""
            SELECT 
                query_text,
                response_text,
                ui_payload,
                intent,
                (embedding <=> CAST(:qvec AS vector)) AS distance
            FROM semantic_query_cache
            WHERE (embedding <=> CAST(:qvec AS vector)) <= :max_dist
            ORDER BY distance ASC
            LIMIT 1;
        """)
        with self.engine.connect() as conn:
            row = conn.execute(query, {"qvec": vec_str, "max_dist": max_distance}).fetchone()
            if not row:
                return None
            dist = float(row[4])
            similarity = round(1.0 - dist, 4)
            return {
                "matched_query": row[0],
                "response_message": row[1],
                "ui_payload": row[2] if isinstance(row[2], dict) else json.loads(row[2]),
                "intent": row[3],
                "cosine_similarity": similarity,
                "is_cache_hit": True,
                "cache_tier": "TIER_1_SEMANTIC_VECTOR",
            }

    def save_semantic_cache_entry(
        self,
        query_text: str,
        query_vec: List[float],
        response_text: str,
        ui_payload: Dict[str, Any],
        intent: str,
    ) -> bool:
        """Stores query embedding and response in semantic_query_cache."""
        vec_str = "[" + ",".join(str(x) for x in query_vec) + "]"
        query = text("""
            INSERT INTO semantic_query_cache (query_text, embedding, response_text, ui_payload, intent)
            VALUES (:qtext, CAST(:qvec AS vector), :resp, CAST(:ui AS jsonb), :intent);
        """)
        try:
            with self.engine.begin() as conn:
                conn.execute(query, {
                    "qtext": query_text,
                    "qvec": vec_str,
                    "resp": response_text,
                    "ui": json.dumps(ui_payload),
                    "intent": intent,
                })
            return True
        except Exception as e:
            logger.debug(f"[CACHE: SEMANTIC] Failed to insert semantic cache entry: {e}")
            return False