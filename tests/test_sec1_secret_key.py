import pytest
from pydantic import ValidationError

from core.config import DEFAULT_SECRET_KEY, Settings


STRONG = {"SECRET_KEY": "x" * 40, "POSTGRES_PASSWORD": "pg-" + "y" * 20, "MINIO_ROOT_PASSWORD": "mn-" + "z" * 20}


def _make(**kw):
    return Settings(_env_file=None, **kw)


def test_default_secret_allowed_in_development():
    assert _make(ENVIRONMENT="development", SECRET_KEY=DEFAULT_SECRET_KEY).SECRET_KEY == DEFAULT_SECRET_KEY


def test_default_secret_rejected_in_production():
    with pytest.raises(ValidationError):
        _make(ENVIRONMENT="production", SECRET_KEY=DEFAULT_SECRET_KEY)


def test_custom_secret_allowed_in_production():
    assert _make(ENVIRONMENT="production", **STRONG).ENVIRONMENT == "production"


@pytest.mark.parametrize("field", ["POSTGRES_PASSWORD", "MINIO_ROOT_PASSWORD"])
def test_core12_default_service_password_rejected_in_production(field):
    from core import config

    defaults = {
        "POSTGRES_PASSWORD": config.DEFAULT_POSTGRES_PASSWORD,
        "MINIO_ROOT_PASSWORD": config.DEFAULT_MINIO_ROOT_PASSWORD,
    }
    with pytest.raises(ValidationError, match=field):
        _make(ENVIRONMENT="production", **{**STRONG, field: defaults[field]})


def test_core12_default_service_passwords_allowed_in_development():
    s = _make(ENVIRONMENT="development", SECRET_KEY="x" * 40)
    assert s.POSTGRES_PASSWORD and s.MINIO_ROOT_PASSWORD


def test_core12_custom_service_passwords_allowed_in_production():
    assert _make(ENVIRONMENT="production", **STRONG).ENVIRONMENT == "production"
