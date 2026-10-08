import pytest
from fastapi.testclient import TestClient
from apps.api.main import app
from core.exceptions import NotFoundError, DataUnavailableError, ValidationError


def test_cors_preflight_headers():
    client = TestClient(app)
    # Preflight request from allowed origin
    resp = client.options(
        "/health",
        headers={
            "Origin": "http://localhost:3000",
            "Access-Control-Request-Method": "GET",
        },
    )
    assert resp.status_code == 200
    assert resp.headers.get("access-control-allow-origin") == "http://localhost:3000"


def test_value_error_exception_handler(monkeypatch):
    @app.get("/test-value-error-endpoint")
    def _endpoint():
        raise ValueError("Invalid parameter provided")

    client = TestClient(app)
    resp = client.get("/test-value-error-endpoint")
    assert resp.status_code == 400
    data = resp.json()
    assert "Invalid parameter provided" in data["detail"]


def test_domain_not_found_exception_handler():
    @app.get("/test-not-found-endpoint")
    def _endpoint():
        raise NotFoundError("Resource was not found")

    client = TestClient(app)
    resp = client.get("/test-not-found-endpoint")
    assert resp.status_code == 404
    data = resp.json()
    assert "Resource was not found" in data["detail"]


def test_domain_data_unavailable_exception_handler():
    @app.get("/test-data-unavailable-endpoint")
    def _endpoint():
        raise DataUnavailableError("Data currently unavailable")

    client = TestClient(app)
    resp = client.get("/test-data-unavailable-endpoint")
    assert resp.status_code == 404
    data = resp.json()
    assert "Data currently unavailable" in data["detail"]
