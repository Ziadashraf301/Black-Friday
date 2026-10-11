import pytest
from core.security import (
    hash_password,
    verify_password,
    verify_password_with_upgrade,
    legacy_hash_password,
)


def test_hash_password_supports_long_passwords():
    long_pw = "A" * 150
    hashed = hash_password(long_pw)
    assert verify_password(long_pw, hashed) is True
    assert verify_password("A" * 149 + "B", hashed) is False


def test_hash_password_unicode():
    unicode_pw = "🔑SuperS3cretPasswørd!🎉" * 5
    hashed = hash_password(unicode_pw)
    assert verify_password(unicode_pw, hashed) is True
    assert verify_password(unicode_pw + "x", hashed) is False


def test_verify_password_with_upgrade_for_legacy():
    pw = "legacyPassword123!"
    legacy_hash = legacy_hash_password(pw)
    
    valid, needs_upgrade = verify_password_with_upgrade(pw, legacy_hash)
    assert valid is True
    assert needs_upgrade is True


def test_verify_password_with_upgrade_for_new_hash():
    pw = "newPassword123!"
    new_hash = hash_password(pw)
    
    valid, needs_upgrade = verify_password_with_upgrade(pw, new_hash)
    assert valid is True
    assert needs_upgrade is False


def test_verify_password_with_upgrade_invalid_password():
    pw = "correct_password"
    new_hash = hash_password(pw)
    
    valid, needs_upgrade = verify_password_with_upgrade("wrong_password", new_hash)
    assert valid is False
    assert needs_upgrade is False
