import pytest
from fastapi.testclient import TestClient

from apps.api.client_identity import DEFAULT_SESSION_ID, guest_user_id, sanitize_session_id
from apps.api.main import app
from apps.api.routes import bot


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
