import pytest
from fastapi.testclient import TestClient
from starlette.requests import Request

from apps.api.client_identity import DEFAULT_SESSION_ID, guest_user_id, resolve_client_identity, sanitize_session_id
from apps.api.main import app
from apps.api.rate_limiting.rate_limiter import extract_client_identifier
from apps.api.routes import bot
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
