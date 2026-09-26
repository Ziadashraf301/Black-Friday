"""
Core Security Module — password hashing and JWT token management.
Decoupled from FastAPI web framework dependencies.
"""
from datetime import datetime, timedelta, timezone
from typing import Dict, Any, Optional

from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)

# JWT library check
try:
    from jose import JWTError, jwt as jose_jwt
    HAS_JOSE = True
except ImportError:
    HAS_JOSE = False

# bcrypt library check
try:
    import bcrypt
    HAS_BCRYPT = True
except ImportError:
    HAS_BCRYPT = False


def hash_password(plain: str) -> str:
    """Hashes a plain text password with bcrypt."""
    if not HAS_BCRYPT:
        raise RuntimeError("bcrypt library is required for password hashing.")
    pw_bytes = plain.encode("utf-8")[:72]
    return bcrypt.hashpw(pw_bytes, bcrypt.gensalt()).decode("utf-8")


def verify_password(plain: str, hashed: str) -> bool:
    """Verifies a plain text password against a bcrypt hash."""
    if not HAS_BCRYPT:
        return False
    try:
        pw_bytes = plain.encode("utf-8")[:72]
        return bcrypt.checkpw(pw_bytes, hashed.encode("utf-8"))
    except Exception:
        return False


def create_access_token(payload: Dict[str, Any], expires_delta: Optional[timedelta] = None) -> str:
    """Encodes a JWT access token with expiration."""
    if not HAS_JOSE:
        raise RuntimeError("python-jose library is required for JWT creation.")
    data = payload.copy()
    if expires_delta:
        expire = datetime.now(timezone.utc) + expires_delta
    else:
        expire = datetime.now(timezone.utc) + timedelta(hours=settings.JWT_EXPIRY_HOURS)
    data["exp"] = expire
    return jose_jwt.encode(data, settings.SECRET_KEY, algorithm=settings.JWT_ALGORITHM)


def decode_access_token(token: str) -> Dict[str, Any]:
    """Decodes and validates a JWT access token."""
    if not HAS_JOSE:
        raise RuntimeError("python-jose library is required for JWT verification.")
    return jose_jwt.decode(token, settings.SECRET_KEY, algorithms=[settings.JWT_ALGORITHM])
