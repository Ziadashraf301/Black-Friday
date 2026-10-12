"""AI-12: the unused checkpointer / profile store are gone and nothing in the code base still points at them."""

import importlib.util
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]


def test_memory_package_is_gone():
    assert importlib.util.find_spec("ai.memory") is None
    assert not (ROOT / "ai" / "memory").exists()


def test_no_source_references_remain():
    needles = ("ai.memory", "RedisSessionCheckpointer", "UserProfileStore", "user_profile_store", "session_checkpointer")
    for folder in ("ai", "apps", "core", "ml", "evaluation"):
        for path in (ROOT / folder).rglob("*.py"):
            text = path.read_text(encoding="utf-8", errors="ignore")
            assert not any(n in text for n in needles), path


def test_graph_still_compiles_without_a_checkpointer():
    from ai.workflow.graph import build_shopping_graph

    assert build_shopping_graph() is not None
