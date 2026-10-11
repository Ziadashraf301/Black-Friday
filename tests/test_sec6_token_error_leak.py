from fastapi.testclient import TestClient

from apps.api.main import app


def test_invalid_token_error_does_not_leak_exception_text():
    resp = TestClient(app).get("/auth/me", headers={"Authorization": "Bearer not.a.jwt"})
    assert resp.status_code == 401
    body = resp.text
    assert "Invalid or expired token." in body
    for leaked in ("JWTError", "Not enough segments", "Invalid header", "Signature"):
        assert leaked not in body
