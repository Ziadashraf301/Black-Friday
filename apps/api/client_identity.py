"""
Server-verified caller identity shared by the rate limiter, ban middleware and bot routes.
Only a valid Bearer JWT `sub` or the socket peer address is trusted; caller-supplied
X-User-ID headers and user_id query/body parameters are never used.
"""

import hashlib
import hmac
import re
from typing import Any, Optional

from fastapi import Request

from core.config import settings
from core.security import decode_access_token


def verified_user_id(request: Request) -> Optional[str]:
    """Returns the `sub` of a valid Bearer JWT, or None when absent/invalid/expired."""
    auth_header = request.headers.get("Authorization")
    if not auth_header or not auth_header.startswith("Bearer "):
        return None
    try:
        sub = decode_access_token(auth_header.split(" ", 1)[1].strip()).get("sub")
    except Exception:
        return None
    return str(sub) if sub else None


DEFAULT_SESSION_ID = "default_session"
_SESSION_ID_RE = re.compile(r"^[A-Za-z0-9_.:-]{1,64}$")


def client_ip(request: Request) -> str:
    return request.client.host if request.client else "unknown_ip"


def sanitize_session_id(session_id: Any) -> str:
    """Returns the client session id when it is a short safe token, otherwise the default."""
    if isinstance(session_id, str) and _SESSION_ID_RE.match(session_id):
        return session_id
    return DEFAULT_SESSION_ID


def guest_user_id(ip: str, session_id: Any) -> str:
    """
    Per-visitor guest id derived from the peer address and session id with an HMAC keyed by SECRET_KEY.
    Strangers on different addresses never share cart/profile state, ids cannot be guessed or forged
    by a client, and the `guest_` prefix can never collide with a numeric shopper id.
    """
    msg = f"{ip}|{sanitize_session_id(session_id)}".encode("utf-8")
    digest = hmac.new(settings.SECRET_KEY.encode("utf-8"), msg, hashlib.sha256).hexdigest()
    return f"guest_{digest[:24]}"


def bot_user_id(request: Request, session_id: Any = None) -> str:
    """Shopper id for the assistant: verified JWT subject, otherwise a per-session guest id."""
    return verified_user_id(request) or guest_user_id(client_ip(request), session_id)


def resolve_client_identity(request: Request) -> str:
    """Verified JWT subject when present, otherwise the client IP."""
    return verified_user_id(request) or client_ip(request)
