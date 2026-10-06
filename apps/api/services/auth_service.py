"""
Authentication service — user registration, credential validation, and JWT sessions.
"""
from typing import Dict, Any
from fastapi import HTTPException, status

from core.security import hash_password, verify_password, create_access_token, decode_access_token
from core.logging import get_logger
from core.db.repository import BlackFridayRepository

logger = get_logger(__name__)


class AuthService:
    """Enterprise authentication service coordinating user accounts and JWT sessions."""

    hash_password = staticmethod(hash_password)
    verify_password = staticmethod(verify_password)
    create_access_token = staticmethod(create_access_token)
    decode_token = staticmethod(decode_access_token)

    @classmethod
    def register_user(cls, user_data: Dict[str, Any], repo: BlackFridayRepository) -> Dict[str, Any]:
        """Registers a new shopper and generates their session token."""
        try:
            repo.ensure_user_tables()
        except Exception:
            pass

        existing = repo.get_user_by_email(user_data["email"])
        if existing:
            raise HTTPException(
                status_code=status.HTTP_409_CONFLICT,
                detail=f"An account with email '{user_data['email']}' already exists.",
            )

        pw_hash = cls.hash_password(user_data["password"])
        record = {
            "name": user_data["name"],
            "email": user_data["email"],
            "password_hash": pw_hash,
            "gender": user_data.get("gender", "M"),
            "age": user_data.get("age", "26-35"),
            "city_category": user_data.get("city_category", "A"),
            "marital_status": int(user_data.get("marital_status", 0)),
            "occupation": int(user_data.get("occupation", 1)),
            "stay_in_current_city_years": str(user_data.get("stay_in_current_city_years", "2")),
            "cluster_id": 1,
            "cluster_persona": "Preferred Member",
            "recommended_action": "Exclusive member promotions",
        }

        user = repo.create_user(record)
        token = cls.create_access_token({
            "sub": str(user["user_id"]),
            "name": user["name"],
            "email": user["email"],
            "gender": user.get("gender"),
            "age": user.get("age"),
            "city_category": user.get("city_category"),
            "marital_status": user.get("marital_status"),
            "occupation": user.get("occupation"),
            "stay_in_current_city_years": user.get("stay_in_current_city_years"),
        })

        return {
            "access_token": token,
            "token_type": "bearer",
            "user_id": user["user_id"],
            "name": user["name"],
            "cluster_persona": user.get("cluster_persona", "Preferred Member"),
        }

    @classmethod
    def authenticate_user(cls, email: str, password: str, repo: BlackFridayRepository) -> Dict[str, Any]:
        """Validates credentials and returns JWT token."""
        try:
            repo.ensure_user_tables()
        except Exception:
            pass

        user = repo.get_user_by_email(email)
        if not user or not cls.verify_password(password, user["password_hash"]):
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Incorrect email or password.",
                headers={"WWW-Authenticate": "Bearer"},
            )

        token = cls.create_access_token({
            "sub": str(user["user_id"]),
            "name": user["name"],
            "email": user["email"],
            "gender": user.get("gender"),
            "age": user.get("age"),
            "city_category": user.get("city_category"),
            "marital_status": user.get("marital_status"),
            "occupation": user.get("occupation"),
            "stay_in_current_city_years": user.get("stay_in_current_city_years"),
        })

        return {
            "access_token": token,
            "token_type": "bearer",
            "user_id": user["user_id"],
            "name": user["name"],
            "cluster_persona": user.get("cluster_persona", "Preferred Member"),
        }


auth_service = AuthService()
