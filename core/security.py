"""
Core Security Module — password hashing and JWT token management (Fix 2.3).
Decoupled from FastAPI web framework dependencies.
Implements SHA-256 pre-hashing before bcrypt to safely handle arbitrary length
and multi-byte UTF-8 passwords without silent 72-byte truncation.
Provides upgrade-on-login for legacy bcrypt hashes.
"""
import hashlib
from datetime import datetime, timedelta, timezone
from typing import Dict, Any, Optional, Tuple

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


def _prehash_password(plain: str) -> bytes:
    """Pre-hashes plain text password with SHA-256 to produce fixed-length ASCII bytes."""
    return hashlib.sha256(plain.encode("utf-8")).hexdigest().encode("utf-8")


def hash_password(plain: str) -> str:
    """Hashes a plain text password with SHA-256 pre-hashing and bcrypt."""
    if not HAS_BCRYPT:
        raise RuntimeError("bcrypt library is required for password hashing.")
    prehash_bytes = _prehash_password(plain)
    return bcrypt.hashpw(prehash_bytes, bcrypt.gensalt()).decode("utf-8")


def legacy_hash_password(plain: str) -> str:
    """Legacy direct-truncate bcrypt hashing scheme for testing and backward compatibility."""
    if not HAS_BCRYPT:
        raise RuntimeError("bcrypt library is required for password hashing.")
    pw_bytes = plain.encode("utf-8")[:72]
    return bcrypt.hashpw(pw_bytes, bcrypt.gensalt()).decode("utf-8")


def verify_password_with_upgrade(plain: str, hashed: str) -> Tuple[bool, bool]:
    """
    Verifies a plain text password against a hash.
    Checks the new SHA-256 pre-hashed scheme first.
    Falls back to the legacy direct bcrypt scheme.
    Returns: (is_valid, needs_upgrade)
    """
    if not HAS_BCRYPT or not hashed:
        return False, False

    hashed_bytes = hashed.encode("utf-8")

    # 1. Primary check: new SHA-256 pre-hashed scheme
    try:
        if bcrypt.checkpw(_prehash_password(plain), hashed_bytes):
            return True, False
    except Exception:
        pass

    # 2. Fallback check: legacy direct 72-byte truncation scheme
    try:
        legacy_bytes = plain.encode("utf-8")[:72]
        if bcrypt.checkpw(legacy_bytes, hashed_bytes):
            return True, True
    except Exception:
        pass

    return False, False


def verify_password(plain: str, hashed: str) -> bool:
    """Verifies a plain text password against a bcrypt hash (supporting both new and legacy schemes)."""
    valid, _ = verify_password_with_upgrade(plain, hashed)
    return valid


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


__all__ = [
    "hash_password",
    "legacy_hash_password",
    "verify_password",
    "verify_password_with_upgrade",
    "create_access_token",
    "decode_access_token",
]
