"""
User repository for shopper authentication, profiles, and order history.
"""

from typing import Any, Dict, List, Optional

from sqlalchemy import text

from core.db.repositories.base import BaseRepository
from core.logging import get_logger

logger = get_logger(__name__)

# The model was trained on "0", "1", "2", "3", "4+" with no missing category; an unseen string such as "None" is
# ordinal-encoded to -1. "1" is the most common training value (35% of rows) and the batch pricing fallback.
DEFAULT_STAY_IN_CITY_YEARS = "1"


class UserRepository(BaseRepository):
    """Repository managing app_users and user_purchases tables."""

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

    @staticmethod
    def _demographics_from_row(row: Dict[str, Any]) -> Dict[str, Any]:
        """Normalises a user / transaction row into the demographic vector used for pricing."""
        stay = row["stay_in_current_city_years"]
        return {
            "gender": row["gender"],
            "age": row["age"],
            "city_category": row["city_category"],
            "stay_in_current_city_years": str(stay) if stay not in (None, "") else DEFAULT_STAY_IN_CITY_YEARS,
            "marital_status": int(row["marital_status"]) if row["marital_status"] is not None else 0,
            "occupation": int(row["occupation"]) if row["occupation"] is not None else 0,
        }

    def get_user_demographics(self, user_id: int) -> Optional[Dict[str, Any]]:
        """Retrieves exact demographic vector for user_id from app_users or black_friday_cleaned table."""
        # 1. Check app_users table
        user = self.get_user_by_id(user_id)
        if user and user.get("gender") and user.get("age"):
            return self._demographics_from_row(user)

        # 2. Check black_friday_cleaned table
        query = text(
            """
            SELECT gender, age, occupation, city_category, stay_in_current_city_years, marital_status
            FROM black_friday_cleaned
            WHERE user_id = :user_id
            LIMIT 1
        """
        )
        with self.engine.connect() as conn:
            res = conn.execute(query, {"user_id": user_id}).mappings().first()
            return self._demographics_from_row(dict(res)) if res else None

    def create_user(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Insert a new user and return the created record."""
        query = text(
            """
            INSERT INTO app_users
                (name, email, password_hash, gender, age, city_category,
                 marital_status, occupation, stay_in_current_city_years,
                 cluster_id, cluster_persona, recommended_action)
            VALUES
                (:name, :email, :password_hash, :gender, :age, :city_category,
                 :marital_status, :occupation, :stay_in_current_city_years,
                 :cluster_id, :cluster_persona, :recommended_action)
            RETURNING *
        """
        )
        with self.engine.begin() as conn:
            result = conn.execute(query, data).mappings().first()
            return dict(result)

    _INSERT_PURCHASE = text(
        """
        INSERT INTO user_purchases
            (user_id, product_id, product_category_1, product_category_2,
             product_category_3, predicted_usd, model_used)
        VALUES
            (:user_id, :product_id, :product_category_1, :product_category_2,
             :product_category_3, :predicted_usd, :model_used)
        RETURNING *
    """
    )

    def record_purchase(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Save a shopper's product prediction/purchase event."""
        with self.engine.begin() as conn:
            result = conn.execute(self._INSERT_PURCHASE, data).mappings().first()
            return dict(result)

    def record_purchases_batch(self, records: List[Dict[str, Any]], connection: Optional[Any] = None) -> List[Dict[str, Any]]:
        """Save multiple purchases in ONE atomic transaction with rollback on failure."""
        if not records:
            return []

        def insert_all(conn) -> List[Dict[str, Any]]:
            return [dict(conn.execute(self._INSERT_PURCHASE, rec).mappings().first()) for rec in records]

        if connection is not None:
            return insert_all(connection)
        with self.engine.begin() as conn:
            return insert_all(conn)

    def get_user_purchase_history(self, user_id: int) -> List[Dict[str, Any]]:
        query = text(
            """
            SELECT id, product_id, product_category_1, product_category_2,
                   product_category_3, predicted_usd, model_used, purchased_at
            FROM user_purchases
            WHERE user_id = :user_id
            ORDER BY purchased_at DESC
            LIMIT 50
        """
        )
        with self.engine.connect() as conn:
            return [dict(r) for r in conn.execute(query, {"user_id": user_id}).mappings().all()]

    def update_user_password(self, user_id: int, new_password_hash: str) -> None:
        """Update a user's password hash in app_users."""
        query = text(
            """
            UPDATE app_users
            SET password_hash = :password_hash
            WHERE user_id = :user_id
        """
        )
        with self.engine.begin() as conn:
            conn.execute(query, {"user_id": user_id, "password_hash": new_password_hash})
