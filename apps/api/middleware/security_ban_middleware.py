"""
Security Ban & Lockout Gateway Middleware (Phase 4 - Task P4-07, Fix 5.2).
Intercepts incoming API requests and blocks banned users/IPs with HTTP 403 Forbidden.
Inspects the verified Bearer JWT subject and the client IP (caller-supplied IDs are ignored).
"""

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.responses import JSONResponse

from ai.guardrails.strike_tracker import strike_tracker
from apps.api.client_identity import client_ip, verified_user_id
from core.logging import get_logger

logger = get_logger(__name__)


class SecurityBanMiddleware(BaseHTTPMiddleware):
    """
    Gateway protection middleware.
    Inspects user identifier or IP against Redis 3-strike lockout store.
    Decodes the Bearer JWT token to identify the user.
    """

    async def dispatch(self, request: Request, call_next) -> Response:
        path = request.url.path.lower()
        if path.startswith("/shopper") or path.startswith("/bot") or path.startswith("/api/"):
            # Identity is server-verified only: JWT sub (invalid tokens are ignored) and socket IP.
            user_id = verified_user_id(request)
            ip = client_ip(request)

            is_banned = False
            banned_id = None

            if user_id and strike_tracker.is_banned(user_id):
                is_banned = True
                banned_id = user_id
            elif strike_tracker.is_banned(ip):
                is_banned = True
                banned_id = ip

            if is_banned:
                logger.warning(f"[SECURITY: GATEWAY] Rejected HTTP request from locked-out user/IP: {banned_id}")
                return JSONResponse(
                    status_code=403,
                    content={
                        "detail": "Access revoked due to repeated security policy violations. Lockout expires in 24 hours.",
                        "error_code": "SECURITY_STRIKE_LOCKOUT",
                        "lockout_duration_hours": 24,
                    },
                )

        return await call_next(request)


__all__ = ["SecurityBanMiddleware"]
