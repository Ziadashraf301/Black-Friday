"""
FastAPI authentication dependency and security exports.
"""
from typing import Dict, Any
from fastapi import Depends, HTTPException, status
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials

from core.security import hash_password, verify_password, create_access_token, decode_access_token
from apps.api.services.auth_service import auth_service

_bearer_scheme = HTTPBearer(auto_error=True)


def get_current_user(
    credentials: HTTPAuthorizationCredentials = Depends(_bearer_scheme),
) -> Dict[str, Any]:
    """
    FastAPI dependency validating the JWT Bearer token and returning user identity.
    """
    try:
        payload = decode_access_token(credentials.credentials)
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail=f"Invalid or expired token: {e}",
            headers={"WWW-Authenticate": "Bearer"},
        )

    user_id = payload.get("sub")
    if not user_id:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Token missing subject claim.",
            headers={"WWW-Authenticate": "Bearer"},
        )
    return {
        "user_id": int(user_id),
        "name": payload.get("name", ""),
        "email": payload.get("email", ""),
    }
