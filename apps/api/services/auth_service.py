"""
Authentication service — user registration, credential validation, and JWT sessions.
"""

from typing import Any, Dict

from sqlalchemy.exc import IntegrityError

from core.db.repository import BlackFridayRepository
from core.exceptions import ConflictError, UnauthorizedError
from core.logging import get_logger
from core.security import (
    create_access_token,
    decode_access_token,
    hash_password,
    verify_password,
    verify_password_with_upgrade,
)

logger = get_logger(__name__)


def _normalize_email(email: str) -> str:
    return email.strip().lower()


class AuthService:
    """Enterprise authentication service coordinating user accounts and JWT sessions."""

    hash_password = staticmethod(hash_password)
    verify_password = staticmethod(verify_password)
    verify_password_with_upgrade = staticmethod(verify_password_with_upgrade)
    create_access_token = staticmethod(create_access_token)
    decode_token = staticmethod(decode_access_token)

    @classmethod
    def _session_response(cls, user: Dict[str, Any]) -> Dict[str, Any]:
        """Issues a JWT carrying the shopper profile claims and builds the token response."""
        token = cls.create_access_token(
            {
                "sub": str(user["user_id"]),
                "name": user["name"],
                "email": user["email"],
                "gender": user.get("gender"),
                "age": user.get("age"),
                "city_category": user.get("city_category"),
                "marital_status": user.get("marital_status"),
                "occupation": user.get("occupation"),
                "stay_in_current_city_years": user.get("stay_in_current_city_years"),
            }
        )
        return {
            "access_token": token,
            "token_type": "bearer",
            "user_id": user["user_id"],
            "name": user["name"],
            "cluster_persona": user.get("cluster_persona", "Preferred Member"),
        }

    @classmethod
    def register_user(cls, user_data: Dict[str, Any], repo: BlackFridayRepository) -> Dict[str, Any]:
        """Registers a new shopper and generates their session token."""
        email = _normalize_email(user_data["email"])
        conflict = ConflictError(
            message=f"An account with email '{email}' already exists.",
            code="EMAIL_CONFLICT",
        )
        if repo.get_user_by_email(email):
            raise conflict

        pw_hash = cls.hash_password(user_data["password"])
        record = {
            "name": user_data["name"],
            "email": email,
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

        try:
            user = repo.create_user(record)
        except IntegrityError:
            # Concurrent signup with the same email lost the race against the UNIQUE constraint
            raise conflict
        return cls._session_response(user)

    @classmethod
    def authenticate_user(cls, email: str, password: str, repo: BlackFridayRepository) -> Dict[str, Any]:
        """Validates credentials and returns JWT token."""
        normalized = _normalize_email(email)
        # Fall back to the submitted spelling for accounts created before emails were lower-cased
        user = repo.get_user_by_email(normalized) or (
            repo.get_user_by_email(email.strip()) if email.strip() != normalized else None
        )
        if not user:
            raise UnauthorizedError(
                message="Incorrect email or password.",
                code="INVALID_CREDENTIALS",
            )

        valid, needs_upgrade = cls.verify_password_with_upgrade(password, user["password_hash"])
        if not valid:
            raise UnauthorizedError(
                message="Incorrect email or password.",
                code="INVALID_CREDENTIALS",
            )

        if needs_upgrade:
            try:
                new_hash = cls.hash_password(password)
                repo.update_user_password(user["user_id"], new_hash)
                logger.info(f"Upgraded password hash for user_id={user['user_id']}")
            except Exception as e:
                logger.warning(f"Failed to upgrade password hash for user_id={user['user_id']}: {e}")

        return cls._session_response(user)


auth_service = AuthService()
