import pytest
from fastapi.testclient import TestClient

from apps.api.main import app
from apps.api.routes import bot
from apps.api.routes.bot import tokenize_for_stream
from core.security import create_access_token


@pytest.fixture
def captured(monkeypatch):
    seen = []

    async def fake_generator(query, user_id, session_id):
        seen.append(user_id)
        yield "data: [DONE]\n\n"

    monkeypatch.setattr(bot, "sse_response_generator", fake_generator)
    return seen


def _auth(sub):
    return {"Authorization": f"Bearer {create_access_token({'sub': sub})}"}


def test_post_uses_jwt_user_not_body_user_id(captured):
    client = TestClient(app)
    resp = client.post("/bot/stream", json={"query": "hi", "user_id": "victim"}, headers=_auth("77"))
    assert resp.status_code == 200
    assert captured == ["77"]


def test_post_without_token_is_guest_even_if_body_claims_user(captured):
    client = TestClient(app)
    client.post("/bot/stream", json={"query": "hi", "user_id": "victim"})
    assert len(captured) == 1 and captured[0].startswith("guest_")


def test_get_ignores_user_id_query_and_header(captured):
    client = TestClient(app)
    client.get("/bot/stream?query=hi&user_id=victim", headers={"X-User-ID": "victim"})
    assert len(captured) == 1 and captured[0].startswith("guest_")


def test_get_with_invalid_token_is_guest(captured):
    client = TestClient(app)
    client.get("/bot/stream?query=hi", headers={"Authorization": "Bearer garbage"})
    assert len(captured) == 1 and captured[0].startswith("guest_")


def test_tokenize_for_stream_exact_roundtrip():
    text = "Hello world! This is a test.\n\n- Bullet 1\n- Bullet 2\n   Indented text."
    tokens = tokenize_for_stream(text)
    reconstructed = "".join(tokens)
    assert reconstructed == text


def test_tokenize_for_stream_empty_and_spaces():
    assert tokenize_for_stream("") == []
    text = "   "
    assert "".join(tokenize_for_stream(text)) == text


def test_tokenize_for_stream_splits_into_chunks():
    text = "One two three four five"
    tokens = tokenize_for_stream(text)
    assert len(tokens) >= 5
    assert "".join(tokens) == text


class _Graph:
    def __init__(self, fail=False):
        self.fail = fail
        self.queries = []

    def invoke(self, state):
        if self.fail:
            raise RuntimeError("secret internal detail: db password=hunter2")
        self.queries.append(state["query"])
        return {"final_response": "ok", "ui_payload": {}}


def test_non_object_json_frame_is_treated_as_query(monkeypatch):
    graph = _Graph()
    monkeypatch.setattr(bot, "shopping_graph", graph)
    with TestClient(app).websocket_connect("/bot/live-ws") as ws:
        ws.send_text("[1, 2, 3]")
        assert ws.receive_json()["type"] == "voice_response"
        ws.send_text("42")
        assert ws.receive_json()["type"] == "voice_response"
    assert graph.queries == ["[1, 2, 3]", "42"]


def test_internal_error_text_not_sent_to_client(monkeypatch):
    monkeypatch.setattr(bot, "shopping_graph", _Graph(fail=True))
    with TestClient(app).websocket_connect("/bot/live-ws") as ws:
        ws.send_text(__import__("json").dumps({"query": "hi"}))
        msg = ws.receive_json()
    assert msg["type"] == "error"
    assert "hunter2" not in msg["message"] and "internal detail" not in msg["message"]
