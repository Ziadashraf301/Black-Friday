from fastapi.testclient import TestClient

from apps.api.main import app
from apps.api.rate_limiting.rate_limiter import RedisRateLimiter, auth_rate_limiter
from core.config import settings


def test_login_is_throttled_per_ip():
    client = TestClient(app)
    key = "auth:testclient"
    auth_rate_limiter.reset_user(key)
    try:
        codes = [
            client.post("/auth/login", json={"email": "nobody@example.com", "password": "wrong-password"}).status_code
            for _ in range(settings.AUTH_RATE_LIMIT_MINUTE + 2)
        ]
        assert 429 in codes
        assert codes.index(429) == settings.AUTH_RATE_LIMIT_MINUTE
        assert all(c != 429 for c in codes[: settings.AUTH_RATE_LIMIT_MINUTE])
    finally:
        auth_rate_limiter.reset_user(key)


def test_signup_is_throttled_and_spoofed_header_does_not_help():
    client = TestClient(app)
    key = "auth:testclient"
    auth_rate_limiter.reset_user(key)
    try:
        last = None
        for i in range(settings.AUTH_RATE_LIMIT_MINUTE + 1):
            last = client.post("/auth/signup", json={}, headers={"X-User-ID": f"rotating-{i}"}).status_code
        assert last == 429
    finally:
        auth_rate_limiter.reset_user(key)


def test_auth_limiter_is_stricter_and_separate_from_default():
    assert auth_rate_limiter.minute_limit == settings.AUTH_RATE_LIMIT_MINUTE
    assert isinstance(auth_rate_limiter, RedisRateLimiter)
    assert settings.AUTH_RATE_LIMIT_MINUTE < settings.RATE_LIMIT_MINUTE
