"""
FastAPI authentication dependency and security exports.
"""
from typing import Dict, Any, Optional
from fastapi import Depends, HTTPException, status
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials

from core.security import hash_password, verify_password, create_access_token, decode_access_token
from apps.api.services.auth_service import auth_service

_bearer_scheme = HTTPBearer(auto_error=True)
_optional_bearer_scheme = HTTPBearer(auto_error=False)


def _build_user_dict(payload: Dict[str, Any]) -> Dict[str, Any]:
    user_id = payload.get("sub")
    if not user_id:
        raise ValueError("Token missing subject claim.")
    return {
        "user_id": int(user_id),
        "name": payload.get("name", ""),
        "email": payload.get("email", ""),
        "gender": payload.get("gender"),
        "age": payload.get("age"),
        "city_category": payload.get("city_category"),
        "marital_status": payload.get("marital_status"),
        "occupation": payload.get("occupation"),
        "stay_in_current_city_years": payload.get("stay_in_current_city_years"),
    }


def get_current_user(
    credentials: HTTPAuthorizationCredentials = Depends(_bearer_scheme),
) -> Dict[str, Any]:
    """
    FastAPI dependency validating the JWT Bearer token and returning user identity.
    """
    try:
        payload = decode_access_token(credentials.credentials)
        return _build_user_dict(payload)
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail=f"Invalid or expired token: {e}",
            headers={"WWW-Authenticate": "Bearer"},
        )


def get_optional_user(
    credentials: Optional[HTTPAuthorizationCredentials] = Depends(_optional_bearer_scheme),
) -> Optional[Dict[str, Any]]:
    """
    FastAPI dependency allowing optional authentication.
    Returns user dict when a valid Bearer token is provided, or None for guest/unauthenticated calls.
    """
    if credentials is None or not credentials.credentials:
        return None
    try:
        payload = decode_access_token(credentials.credentials)
        return _build_user_dict(payload)
    except Exception:
        return None


__all__ = [
    "get_current_user",
    "get_optional_user",
    "hash_password",
    "verify_password",
    "create_access_token",
    "decode_access_token",
    "auth_service",
]
