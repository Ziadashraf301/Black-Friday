"""
Security Ban & Lockout Gateway Middleware (Phase 4 - Task P4-07).
Intercepts incoming API requests and blocks banned users/IPs with HTTP 403 Forbidden.
"""
from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.responses import JSONResponse
from ai.guardrails.strike_tracker import strike_tracker
from core.logging import get_logger

logger = get_logger(__name__)


class SecurityBanMiddleware(BaseHTTPMiddleware):
    """
    Gateway protection middleware.
    Inspects user identifier or IP against Redis 3-strike lockout store.
    """

    async def dispatch(self, request: Request, call_next) -> Response:
        # Inspect shopper and bot routes
        path = request.url.path.lower()
        if path.startswith("/shopper") or path.startswith("/bot") or path.startswith("/api/"):
            user_id = request.headers.get("X-User-ID") or request.query_params.get("user_id")
            client_ip = request.client.host if request.client else "unknown_ip"
            identifier = user_id or client_ip

            if strike_tracker.is_banned(identifier):
                logger.warning(f"[SECURITY: GATEWAY] Rejected HTTP request from locked-out user/IP: {identifier}")
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
