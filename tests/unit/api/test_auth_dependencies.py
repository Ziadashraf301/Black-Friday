import pytest
from fastapi import HTTPException
from fastapi.testclient import TestClient

from apps.api.auth import get_current_user, get_optional_user
from apps.api.main import app
from core.security import create_access_token


@pytest.mark.slow
def test_get_optional_user_none_when_no_credentials():
    res = get_optional_user(credentials=None)
    assert res is None


def test_get_optional_user_returns_user_with_valid_token():
    token = create_access_token({"sub": "42", "email": "shopper@test.com"})

    class FakeCreds:
        credentials = token

    user = get_optional_user(credentials=FakeCreds())
    assert user is not None
    assert user["user_id"] == 42
    assert user["email"] == "shopper@test.com"


def test_get_optional_user_returns_none_on_invalid_token():
    class FakeCreds:
        credentials = "invalid.bearer.token"

    res = get_optional_user(credentials=FakeCreds())
    assert res is None


def test_invalid_token_error_does_not_leak_exception_text():
    resp = TestClient(app).get("/auth/me", headers={"Authorization": "Bearer not.a.jwt"})
    assert resp.status_code == 401
    body = resp.text
    assert "Invalid or expired token." in body
    for leaked in ("JWTError", "Not enough segments", "Invalid header", "Signature"):
        assert leaked not in body
