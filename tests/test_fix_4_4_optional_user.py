import pytest
from apps.api.auth import get_optional_user, get_current_user
from core.security import create_access_token
from fastapi import HTTPException


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
