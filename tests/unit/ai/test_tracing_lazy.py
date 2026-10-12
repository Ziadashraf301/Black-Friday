"""Importing the tracing module must not contact MLflow; the tracer is built on first use."""

import subprocess
import sys


def test_import_builds_no_tracer_and_first_access_builds_one(monkeypatch):
    import ai.observability.tracing as tracing

    built = []
    monkeypatch.setattr(tracing, "_agent_tracer", None)
    monkeypatch.setattr(tracing.AgentTracer, "__init__", lambda self, *a, **k: built.append(self))
    assert built == []
    first = tracing.agent_tracer
    assert tracing.get_agent_tracer() is first and built == [first]


def test_fresh_import_does_not_construct_the_tracer():
    code = (
        "import ai.observability.tracing as t, sys;"
        "sys.exit(0 if t._agent_tracer is None and 'agent_tracer' not in vars(t) else 1)"
    )
    assert subprocess.run([sys.executable, "-c", code], timeout=120).returncode == 0
