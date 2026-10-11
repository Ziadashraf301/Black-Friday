import pytest
from pydantic import ValidationError

from core.config import DEFAULT_SECRET_KEY, Settings


def _make(**kw):
    return Settings(_env_file=None, **kw)


def test_default_secret_allowed_in_development():
    assert _make(ENVIRONMENT="development", SECRET_KEY=DEFAULT_SECRET_KEY).SECRET_KEY == DEFAULT_SECRET_KEY


def test_default_secret_rejected_in_production():
    with pytest.raises(ValidationError):
        _make(ENVIRONMENT="production", SECRET_KEY=DEFAULT_SECRET_KEY)


def test_custom_secret_allowed_in_production():
    assert _make(ENVIRONMENT="production", SECRET_KEY="x" * 40).ENVIRONMENT == "production"
