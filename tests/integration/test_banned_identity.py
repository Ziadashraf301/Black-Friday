"""A banned user cannot evade the ban with a spoofed header (strike tracker on the live Redis)."""

from starlette.requests import Request

from ai.guardrails.strike_tracker import strike_tracker
from core.security import create_access_token


def _request(headers=None, query="", ip="203.0.113.7") -> Request:
    scope = {
        "type": "http",
        "method": "GET",
        "path": "/bot/stream",
        "query_string": query.encode(),
        "headers": [(k.lower().encode(), v.encode()) for k, v in (headers or {}).items()],
        "client": (ip, 1234),
    }
    return Request(scope)


def test_banned_user_cannot_evade_with_spoofed_header():
    from fastapi.testclient import TestClient

    from apps.api.main import app

    banned = "sec3_banned_user"
    strike_tracker.reset_strikes(banned)
    try:
        for _ in range(3):
            strike_tracker.record_strike(banned)
        token = create_access_token({"sub": banned})
        resp = TestClient(app).get(
            "/shopper/curated-catalog",
            headers={"Authorization": f"Bearer {token}", "X-User-ID": "innocent"},
        )
        assert resp.status_code == 403
        # Spoofing a banned id without a JWT must not lock out an innocent caller
        resp = TestClient(app).get("/shopper/curated-catalog", headers={"X-User-ID": banned})
        assert resp.status_code != 403
    finally:
        strike_tracker.reset_strikes(banned)
