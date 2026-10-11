from starlette.requests import Request

from ai.guardrails.strike_tracker import strike_tracker
from apps.api.client_identity import resolve_client_identity
from apps.api.rate_limiting.rate_limiter import extract_client_identifier
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


def test_identity_uses_verified_jwt_sub():
    token = create_access_token({"sub": "42"})
    assert resolve_client_identity(_request({"Authorization": f"Bearer {token}"})) == "42"


def test_identity_ignores_spoofable_header_and_query():
    req = _request({"X-User-ID": "victim"}, query="user_id=victim")
    assert extract_client_identifier(req) == "203.0.113.7"


def test_identity_ignores_header_when_jwt_present():
    token = create_access_token({"sub": "42"})
    req = _request({"Authorization": f"Bearer {token}", "X-User-ID": "other"})
    assert extract_client_identifier(req) == "42"


def test_invalid_token_falls_back_to_ip():
    assert resolve_client_identity(_request({"Authorization": "Bearer garbage"})) == "203.0.113.7"


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
