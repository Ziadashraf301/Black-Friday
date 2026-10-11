import pytest
from fastapi.testclient import TestClient

from apps.api.main import app
from apps.api.routes import bot
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
    assert captured == ["guest_user"]


def test_get_ignores_user_id_query_and_header(captured):
    client = TestClient(app)
    client.get("/bot/stream?query=hi&user_id=victim", headers={"X-User-ID": "victim"})
    assert captured == ["guest_user"]


def test_get_with_invalid_token_is_guest(captured):
    client = TestClient(app)
    client.get("/bot/stream?query=hi", headers={"Authorization": "Bearer garbage"})
    assert captured == ["guest_user"]
