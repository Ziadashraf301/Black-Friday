"""
User repository for shopper authentication, profiles, and order history.
"""
from typing import Dict, Any, List, Optional
from sqlalchemy import text
from core.db.repositories.base import BaseRepository
from core.logging import get_logger

logger = get_logger(__name__)


class UserRepository(BaseRepository):
    """Repository managing app_users and user_purchases tables."""

    def ensure_user_tables(self):
        """Create app_users and user_purchases tables if they do not exist."""
        ddl = """
            CREATE TABLE IF NOT EXISTS app_users (
                user_id                   SERIAL PRIMARY KEY,
                name                      TEXT NOT NULL,
                email                     TEXT UNIQUE NOT NULL,
                password_hash             TEXT NOT NULL,
                gender                    VARCHAR(1),
                age                       VARCHAR(10),
                city_category             VARCHAR(1),
                marital_status            INTEGER,
                occupation                INTEGER,
                stay_in_current_city_years VARCHAR(5),
                cluster_id                INTEGER,
                cluster_persona           TEXT,
                recommended_action        TEXT,
                created_at                TIMESTAMPTZ DEFAULT NOW()
            );

            CREATE TABLE IF NOT EXISTS user_purchases (
                id                   SERIAL PRIMARY KEY,
                user_id              INTEGER REFERENCES app_users(user_id) ON DELETE CASCADE,
                product_id           TEXT NOT NULL,
                product_category_1   INTEGER,
                product_category_2   INTEGER,
                product_category_3   INTEGER,
                predicted_usd        FLOAT,
                model_used           TEXT,
                purchased_at         TIMESTAMPTZ DEFAULT NOW()
            );
        """
        with self.engine.begin() as conn:
            conn.execute(text(ddl))
        logger.info("App tables (app_users, user_purchases) ensured.")

    def get_user_by_email(self, email: str) -> Optional[Dict[str, Any]]:
        query = text("SELECT * FROM app_users WHERE email = :email")
        with self.engine.connect() as conn:
            result = conn.execute(query, {"email": email}).mappings().first()
            return dict(result) if result else None

    def get_user_by_id(self, user_id: int) -> Optional[Dict[str, Any]]:
        query = text("SELECT * FROM app_users WHERE user_id = :user_id")
        with self.engine.connect() as conn:
            result = conn.execute(query, {"user_id": user_id}).mappings().first()
            return dict(result) if result else None

    def get_user_demographics(self, user_id: int) -> Optional[Dict[str, Any]]:
        """Retrieves exact demographic vector for user_id from app_users or black_friday_cleaned table."""
        # 1. Check app_users table
        user = self.get_user_by_id(user_id)
        if user and user.get("gender") and user.get("age"):
            return {
                "gender": user["gender"],
                "age": user["age"],
                "city_category": user["city_category"],
                "stay_in_current_city_years": str(user["stay_in_current_city_years"]),
                "marital_status": int(user["marital_status"]) if user["marital_status"] is not None else 0,
                "occupation": int(user["occupation"]) if user["occupation"] is not None else 0,
            }

        # 2. Check black_friday_cleaned table
        query = text("""
            SELECT gender, age, occupation, city_category, stay_in_current_city_years, marital_status
            FROM black_friday_cleaned
            WHERE user_id = :user_id
            LIMIT 1
        """)
        with self.engine.connect() as conn:
            res = conn.execute(query, {"user_id": user_id}).mappings().first()
            if res:
                d = dict(res)
                return {
                    "gender": d["gender"],
                    "age": d["age"],
                    "city_category": d["city_category"],
                    "stay_in_current_city_years": str(d["stay_in_current_city_years"]),
                    "marital_status": int(d["marital_status"]) if d["marital_status"] is not None else 0,
                    "occupation": int(d["occupation"]) if d["occupation"] is not None else 0,
                }
        return None

    def create_user(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Insert a new user and return the created record."""
        query = text("""
            INSERT INTO app_users
                (name, email, password_hash, gender, age, city_category,
                 marital_status, occupation, stay_in_current_city_years,
                 cluster_id, cluster_persona, recommended_action)
            VALUES
                (:name, :email, :password_hash, :gender, :age, :city_category,
                 :marital_status, :occupation, :stay_in_current_city_years,
                 :cluster_id, :cluster_persona, :recommended_action)
            RETURNING *
        """)
        with self.engine.begin() as conn:
            result = conn.execute(query, data).mappings().first()
            return dict(result)

    def record_purchase(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Save a shopper's product prediction/purchase event."""
        query = text("""
            INSERT INTO user_purchases
                (user_id, product_id, product_category_1, product_category_2,
                 product_category_3, predicted_usd, model_used)
            VALUES
                (:user_id, :product_id, :product_category_1, :product_category_2,
                 :product_category_3, :predicted_usd, :model_used)
            RETURNING *
        """)
        with self.engine.begin() as conn:
            result = conn.execute(query, data).mappings().first()
            return dict(result)

    def record_purchases_batch(
        self, records: List[Dict[str, Any]], connection: Optional[Any] = None
    ) -> List[Dict[str, Any]]:
        """Save multiple purchases in ONE atomic transaction with rollback on failure."""
        if not records:
            return []

        query = text("""
            INSERT INTO user_purchases
                (user_id, product_id, product_category_1, product_category_2,
                 product_category_3, predicted_usd, model_used)
            VALUES
                (:user_id, :product_id, :product_category_1, :product_category_2,
                 :product_category_3, :predicted_usd, :model_used)
            RETURNING *
        """)

        if connection is not None:
            results = []
            for rec in records:
                row = connection.execute(query, rec).mappings().first()
                results.append(dict(row))
            return results

        with self.engine.begin() as conn:
            results = []
            for rec in records:
                row = conn.execute(query, rec).mappings().first()
                results.append(dict(row))
            return results


    def get_user_purchase_history(self, user_id: int) -> List[Dict[str, Any]]:
        query = text("""
            SELECT id, product_id, product_category_1, product_category_2,
                   product_category_3, predicted_usd, model_used, purchased_at
            FROM user_purchases
            WHERE user_id = :user_id
            ORDER BY purchased_at DESC
            LIMIT 50
        """)
        with self.engine.connect() as conn:
            return [dict(r) for r in conn.execute(query, {"user_id": user_id}).mappings().all()]

    def update_user_password(self, user_id: int, new_password_hash: str) -> None:
        """Update a user's password hash in app_users."""
        query = text("""
            UPDATE app_users
            SET password_hash = :password_hash
            WHERE user_id = :user_id
        """)
        with self.engine.begin() as conn:
            conn.execute(query, {"user_id": user_id, "password_hash": new_password_hash})

