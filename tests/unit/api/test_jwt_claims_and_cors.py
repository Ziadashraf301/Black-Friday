from datetime import datetime, timedelta, timezone

import pytest
from fastapi.testclient import TestClient
from jose import jwt as jose_jwt

from apps.api.main import app
from core.config import settings
from core.security import create_access_token, decode_access_token


def _raw_token(**claims):
    data = {"sub": "1", "exp": datetime.now(timezone.utc) + timedelta(hours=1), **claims}
    return jose_jwt.encode(data, settings.SECRET_KEY, algorithm=settings.JWT_ALGORITHM)


def test_issued_token_carries_iss_and_aud():
    payload = decode_access_token(create_access_token({"sub": "5"}))
    assert payload["iss"] == settings.JWT_ISSUER
    assert payload["aud"] == settings.JWT_AUDIENCE


def test_legacy_token_without_claims_is_rejected():
    with pytest.raises(Exception):
        decode_access_token(_raw_token())


def test_wrong_issuer_or_audience_is_rejected():
    with pytest.raises(Exception):
        decode_access_token(_raw_token(iss="evil", aud=settings.JWT_AUDIENCE))
    with pytest.raises(Exception):
        decode_access_token(_raw_token(iss=settings.JWT_ISSUER, aud="other"))


def test_cors_preflight_uses_explicit_lists():
    client = TestClient(app)
    origin = "http://localhost:3000"
    ok = client.options(
        "/shopper/purchase",
        headers={
            "Origin": origin,
            "Access-Control-Request-Method": "POST",
            "Access-Control-Request-Headers": "Authorization, Content-Type",
        },
    )
    assert ok.status_code == 200
    assert ok.headers["access-control-allow-origin"] == origin
    assert "*" not in ok.headers["access-control-allow-methods"]
    assert "*" not in ok.headers["access-control-allow-headers"]
    bad = client.options(
        "/shopper/purchase",
        headers={"Origin": origin, "Access-Control-Request-Method": "DELETE"},
    )
    assert bad.status_code == 400
