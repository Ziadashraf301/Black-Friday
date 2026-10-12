"""Login throttling through the real auth route (the login reaches PostgreSQL)."""

import time

import pytest
from fastapi import HTTPException
from fastapi.testclient import TestClient

from apps.api.main import app
from apps.api.rate_limiting.rate_limiter import LUA_SLIDING_WINDOW_RATE_LIMIT, RedisRateLimiter, auth_rate_limiter
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
