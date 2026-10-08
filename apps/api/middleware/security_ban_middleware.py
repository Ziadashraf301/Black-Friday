"""
Security Ban & Lockout Gateway Middleware (Phase 4 - Task P4-07, Fix 5.2).
Intercepts incoming API requests and blocks banned users/IPs with HTTP 403 Forbidden.
Inspects X-User-ID header, query parameter, Bearer JWT token, or client IP.
"""
from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.responses import JSONResponse
from ai.guardrails.strike_tracker import strike_tracker
from core.security import decode_access_token
from core.logging import get_logger

logger = get_logger(__name__)


class SecurityBanMiddleware(BaseHTTPMiddleware):
    """
    Gateway protection middleware.
    Inspects user identifier or IP against Redis 3-strike lockout store.
    Decodes Bearer JWT token to identify the user if X-User-ID is not provided.
    """

    async def dispatch(self, request: Request, call_next) -> Response:
        path = request.url.path.lower()
        if path.startswith("/shopper") or path.startswith("/bot") or path.startswith("/api/"):
            user_id = request.headers.get("X-User-ID") or request.query_params.get("user_id")

            # Extract user_id from Bearer token if not explicitly passed
            if not user_id:
                auth_header = request.headers.get("Authorization")
                if auth_header and auth_header.startswith("Bearer "):
                    token = auth_header.split(" ", 1)[1].strip()
                    try:
                        payload = decode_access_token(token)
                        if payload.get("sub"):
                            user_id = str(payload.get("sub"))
                    except Exception as e:
                        # Malformed or expired JWT must not crash the middleware
                        logger.debug(f"[SECURITY: GATEWAY] Token decoding failed in middleware: {e}")

            client_ip = request.client.host if request.client else "unknown_ip"

            is_banned = False
            banned_id = None

            if user_id and strike_tracker.is_banned(str(user_id)):
                is_banned = True
                banned_id = str(user_id)
            elif client_ip and strike_tracker.is_banned(client_ip):
                is_banned = True
                banned_id = client_ip

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
