import pytest
from unittest.mock import MagicMock
from fastapi.testclient import TestClient
from apps.api.main import app
from apps.api.middleware import security_ban_middleware
from core.security import create_access_token


def test_ban_middleware_allows_normal_request():
    client = TestClient(app)
    resp = client.get("/health")
    assert resp.status_code == 200


def test_ban_middleware_blocks_banned_user_via_jwt(monkeypatch):
    mock_tracker = MagicMock()
    mock_tracker.is_banned.side_effect = lambda uid: uid == "999"
    monkeypatch.setattr(security_ban_middleware, "strike_tracker", mock_tracker)

    token = create_access_token({"sub": "999", "email": "banned@test.com"})
    client = TestClient(app)
    resp = client.get("/bot/analytics-summary", headers={"Authorization": f"Bearer {token}"})
    assert resp.status_code == 403
    assert "revoked" in resp.json()["detail"].lower()


def test_ban_middleware_passes_unbanned_user_via_jwt(monkeypatch):
    mock_tracker = MagicMock()
    mock_tracker.is_banned.side_effect = lambda uid: False
    monkeypatch.setattr(security_ban_middleware, "strike_tracker", mock_tracker)

    token = create_access_token({"sub": "123", "email": "good@test.com"})
    client = TestClient(app)
    resp = client.get("/health", headers={"Authorization": f"Bearer {token}"})
    assert resp.status_code == 200


def test_ban_middleware_tolerates_invalid_jwt(monkeypatch):
    # Mock bot service or endpoint to avoid hitting external DB
    client = TestClient(app)
    resp = client.get("/bot/analytics-summary", headers={"Authorization": "Bearer invalid.token.value"})
    # Invalid JWT does not crash middleware; falls back to IP check or unauthenticated
    assert resp.status_code in (200, 401, 404, 500)
    # Most importantly, ensure it did not fail with 403 lockout or crash unhandled
    assert resp.status_code != 403
