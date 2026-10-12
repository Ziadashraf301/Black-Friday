import asyncio

import pytest
from fastapi.testclient import TestClient
from starlette.requests import Request
from starlette.responses import JSONResponse
from starlette.websockets import WebSocket

from apps.api.client_identity import (
    DEFAULT_SESSION_ID,
    client_ip,
    guest_user_id,
    resolve_client_identity,
    sanitize_session_id,
)
from apps.api.main import app
from apps.api.middleware import security_ban_middleware
from apps.api.rate_limiting.rate_limiter import extract_client_identifier
from apps.api.routes import bot
from core.config import settings
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


@pytest.fixture
def proxies(monkeypatch):
    def _set(value):
        monkeypatch.setattr(settings, "TRUSTED_PROXIES", value)

    return _set


def test_xff_ignored_by_default():
    req = _request({"X-Forwarded-For": "198.51.100.1"}, ip="10.0.0.5")
    assert client_ip(req) == "10.0.0.5"


def test_xff_ignored_from_untrusted_peer(proxies):
    proxies("10.0.0.0/8")
    req = _request({"X-Forwarded-For": "198.51.100.1"}, ip="203.0.113.7")
    assert client_ip(req) == "203.0.113.7"
    assert extract_client_identifier(req) == "203.0.113.7"


def test_xff_used_from_trusted_proxy(proxies):
    proxies("10.0.0.5")
    req = _request({"X-Forwarded-For": "198.51.100.1"}, ip="10.0.0.5")
    assert client_ip(req) == "198.51.100.1"


def test_xff_spoofed_left_entries_ignored(proxies):
    # The client prepends a fake hop; the proxy appends the real peer. The rightmost untrusted hop wins.
    proxies("10.0.0.0/8")
    req = _request({"X-Forwarded-For": "1.2.3.4, 198.51.100.1, 10.0.0.9"}, ip="10.0.0.5")
    assert client_ip(req) == "198.51.100.1"


def test_xff_multiple_headers_and_ipv6(proxies):
    proxies("10.0.0.0/8, ::1")
    scope_headers = [(b"x-forwarded-for", b"1.2.3.4"), (b"x-forwarded-for", b"2001:db8::1")]
    req = Request(
        {"type": "http", "method": "GET", "path": "/", "query_string": b"", "headers": scope_headers, "client": ("::1", 1)}
    )
    assert client_ip(req) == "2001:db8::1"


@pytest.mark.parametrize("xff", ["garbage", "1.2.3.4, not-an-ip", ""])
def test_xff_malformed_falls_back_to_trusted_hop(proxies, xff):
    proxies("10.0.0.5")
    assert client_ip(_request({"X-Forwarded-For": xff}, ip="10.0.0.5")) == "10.0.0.5"


def test_xff_all_trusted_returns_leftmost(proxies):
    proxies("10.0.0.0/8")
    assert client_ip(_request({"X-Forwarded-For": "10.0.0.1, 10.0.0.2"}, ip="10.0.0.5")) == "10.0.0.1"


def test_invalid_trusted_entry_ignored(proxies):
    proxies("not-a-net, 10.0.0.5")
    assert client_ip(_request({"X-Forwarded-For": "198.51.100.1"}, ip="10.0.0.5")) == "198.51.100.1"


@pytest.mark.parametrize("peer,expected", [("10.0.0.5", 403), ("203.0.113.7", 200)])
def test_ban_middleware_uses_shared_client_ip(proxies, monkeypatch, peer, expected):
    # Banned client behind the trusted proxy is blocked; the same header from an untrusted peer is ignored.
    proxies("10.0.0.5")
    monkeypatch.setattr(security_ban_middleware.strike_tracker, "is_banned", lambda ident: ident == "198.51.100.1")
    middleware = security_ban_middleware.SecurityBanMiddleware(app=None)

    async def call_next(request):
        return JSONResponse({}, status_code=200)

    req = _request({"X-Forwarded-For": "198.51.100.1"}, ip=peer)
    resp = asyncio.run(middleware.dispatch(req, call_next))
    assert resp.status_code == expected


def test_websocket_uses_shared_client_ip(proxies):
    proxies("10.0.0.5")
    ws = WebSocket(
        {
            "type": "websocket",
            "path": "/bot/live-ws",
            "query_string": b"",
            "headers": [(b"x-forwarded-for", b"198.51.100.1")],
            "client": ("10.0.0.5", 1),
        },
        receive=None,
        send=None,
    )
    assert client_ip(ws) == "198.51.100.1"


@pytest.fixture
def captured(monkeypatch):
    seen = []

    async def fake_generator(query, user_id, session_id):
        seen.append((user_id, session_id))
        yield "data: [DONE]\n\n"

    monkeypatch.setattr(bot, "sse_response_generator", fake_generator)
    return seen


def test_guest_id_differs_per_ip_and_session():
    a = guest_user_id("1.1.1.1", "s1")
    assert a == guest_user_id("1.1.1.1", "s1")
    assert a != guest_user_id("2.2.2.2", "s1")
    assert a != guest_user_id("1.1.1.1", "s2")
    assert a.startswith("guest_") and not a.isdigit()


@pytest.mark.parametrize("bad", [None, 5, "", "x" * 65, "a b", "../etc", "a\nb"])
def test_sanitize_session_id_rejects_unsafe(bad):
    assert sanitize_session_id(bad) == DEFAULT_SESSION_ID


def test_sanitize_session_id_keeps_safe():
    assert sanitize_session_id("session_guest_user-1.a:b") == "session_guest_user-1.a:b"


def test_get_guest_ids_differ_by_session(captured):
    client = TestClient(app)
    client.get("/bot/stream?query=hi&session_id=aaa")
    client.get("/bot/stream?query=hi&session_id=bbb")
    assert captured[0][0] != captured[1][0]


def test_post_guest_ids_differ_by_session(captured):
    client = TestClient(app)
    client.post("/bot/stream", json={"query": "hi", "session_id": "aaa"})
    client.post("/bot/stream", json={"query": "hi", "session_id": "bbb"})
    assert captured[0][0] != captured[1][0]


def test_invalid_session_id_rejected(captured):
    client = TestClient(app)
    assert client.get("/bot/stream?query=hi&session_id=bad%20id").status_code == 422
    assert client.post("/bot/stream", json={"query": "hi", "session_id": "x" * 65}).status_code == 422
    assert captured == []
