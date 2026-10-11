from fastapi.testclient import TestClient

from apps.api.main import app
from apps.api.routes import bot


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
