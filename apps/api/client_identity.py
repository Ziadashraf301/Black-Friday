"""
Server-verified caller identity shared by the rate limiter, ban middleware and bot routes.
Only a valid Bearer JWT `sub` or the socket peer address is trusted; caller-supplied
X-User-ID headers and user_id query/body parameters are never used.
"""

from typing import Optional

from fastapi import Request

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


GUEST_USER_ID = "guest_user"


def client_ip(request: Request) -> str:
    return request.client.host if request.client else "unknown_ip"


def bot_user_id(request: Request) -> str:
    """Shopper id for the assistant: verified JWT subject, otherwise the shared guest id."""
    return verified_user_id(request) or GUEST_USER_ID


def resolve_client_identity(request: Request) -> str:
    """Verified JWT subject when present, otherwise the client IP."""
    return verified_user_id(request) or client_ip(request)
