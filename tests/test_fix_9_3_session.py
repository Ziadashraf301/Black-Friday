"""
Regression test for Fix 9.3:
- Verify dead code get_db() was removed from core.db.session
- Verify get_db_session() and get_db_engine() remain functional
"""
import core.db.session as session_module
from core.db.session import get_db_engine, get_db_session
from sqlalchemy.orm import Session


def test_get_db_is_removed():
    """Verify dead function get_db is not exported or defined in core.db.session."""
    assert not hasattr(session_module, "get_db"), (
        "Dead function get_db() is still present in core.db.session"
    )
    if hasattr(session_module, "__all__"):
        assert "get_db" not in session_module.__all__


def test_get_db_session_and_engine_work():
    """Verify get_db_session context manager and get_db_engine operate correctly."""
    engine = get_db_engine()
    assert engine is not None

    with get_db_session() as session:
        assert isinstance(session, Session)
