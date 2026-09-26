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

    def create_app_tables(self):
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
