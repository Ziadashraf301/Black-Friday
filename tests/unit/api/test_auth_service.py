from unittest.mock import MagicMock

import pytest
from sqlalchemy.exc import IntegrityError

from apps.api.services.auth_service import AuthService
from core.exceptions import ConflictError, UnauthorizedError

SIGNUP = {"name": "A B", "email": "  Mixed@Case.COM ", "password": "secret123"}


def _repo(existing=None):
    repo = MagicMock()
    repo.get_user_by_email.return_value = existing
    repo.create_user.side_effect = lambda rec: {**rec, "user_id": 1}
    return repo


@pytest.mark.slow
def test_signup_stores_normalized_email_and_checks_normalized():
    repo = _repo()
    out = AuthService.register_user(dict(SIGNUP), repo)
    repo.get_user_by_email.assert_called_with("mixed@case.com")
    assert repo.create_user.call_args[0][0]["email"] == "mixed@case.com"
    assert out["user_id"] == 1


def test_signup_duplicate_raises_conflict():
    with pytest.raises(ConflictError):
        AuthService.register_user(dict(SIGNUP), _repo(existing={"user_id": 9}))


def test_signup_race_integrity_error_becomes_conflict():
    repo = _repo()
    repo.create_user.side_effect = IntegrityError("insert", {}, Exception("dup"))
    with pytest.raises(ConflictError):
        AuthService.register_user(dict(SIGNUP), repo)


def test_login_falls_back_to_legacy_mixed_case_account():
    legacy = {"user_id": 3, "name": "L", "email": "Old@Mail.com",
              "password_hash": AuthService.hash_password("pw123456")}
    repo = MagicMock()
    repo.get_user_by_email.side_effect = lambda e: legacy if e == "Old@Mail.com" else None
    out = AuthService.authenticate_user("Old@Mail.com", "pw123456", repo)
    assert out["user_id"] == 3


def test_login_unknown_email_unauthorized():
    repo = MagicMock()
    repo.get_user_by_email.return_value = None
    with pytest.raises(UnauthorizedError):
        AuthService.authenticate_user("X@y.com", "pw", repo)
