import pytest
from fastapi.testclient import TestClient

from apps.api.main import app
from apps.api.services import shopper_service as shopper_singleton
from apps.api.services.model_service import model_service
from core.exceptions import AppException, DomainError, ValidationError

client = TestClient(app, raise_server_exceptions=False)


@app.get("/test-api8-domain-error")
def _domain_error_endpoint():
    raise DomainError("Cart is empty, add an item first.", code="CART_EMPTY")


@app.get("/test-api8-secret-value-error")
def _secret_value_error_endpoint():
    raise ValueError("password=hunter2 in /srv/secret.cfg")


def test_domain_error_message_is_returned_verbatim():
    resp = client.get("/test-api8-domain-error")
    assert resp.status_code == 400
    assert resp.json() == {"detail": "Cart is empty, add an item first.", "error_type": "CART_EMPTY"}


def test_bare_value_error_is_generic():
    resp = client.get("/test-api8-secret-value-error")
    assert resp.status_code == 400
    assert "hunter2" not in resp.text and "secret.cfg" not in resp.text


def _estimate(monkeypatch, exc):
    def boom(**kw):
        raise exc

    monkeypatch.setattr(model_service, "predict_price", boom)
    repo = type("R", (), {"get_user_demographics": lambda self, uid: None})()
    return shopper_singleton.estimate_price(
        product_id="P1",
        cat1=1,
        cat2=None,
        cat3=None,
        repo=repo,
        gender="M",
        age="26-35",
        occupation=1,
        city_category="A",
        stay_in_current_city_years="1",
        marital_status=0,
    )


def test_domain_error_from_model_surfaces_message(monkeypatch):
    with pytest.raises(ValidationError) as ei:
        _estimate(monkeypatch, DomainError("Missing required feature(s): ['age']"))
    assert "Missing required feature" in ei.value.message


def test_plain_value_error_and_runtime_error_are_generic(monkeypatch):
    with pytest.raises(ValidationError) as ei:
        _estimate(monkeypatch, ValueError("onnx tensor shape (3,) at /models/x.onnx"))
    assert "onnx" not in ei.value.message
    with pytest.raises(AppException) as ei:
        _estimate(monkeypatch, RuntimeError("Predictor failed at /models/x.onnx"))
    assert "/models" not in ei.value.message and ei.value.code == "INFERENCE_FAILURE"


def test_model_service_missing_features_is_domain_error(monkeypatch):
    monkeypatch.setattr(model_service, "predictor", object())
    with pytest.raises(DomainError):
        model_service.predict_price(
            product_id="P1",
            cat1=1,
            cat2=None,
            cat3=None,
            gender=None,
            age=None,
            occupation=None,
            city_category=None,
            stay_in_current_city_years=None,
            marital_status=None,
        )
