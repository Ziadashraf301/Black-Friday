"""AI-1: every ai.* entry point must import cleanly in a fresh interpreter, whatever is imported first."""
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parent.parent
ENTRY_POINTS = [
    "ai.nodes",
    "ai.nodes.router_node",
    "ai.nodes.aggregator_node",
    "ai.workflow",
    "ai.workflow.graph",
    "ai.workflow.state",
    "ai.schemas.state",
    "ai.services.synthesis_service",
]


@pytest.mark.parametrize("module", ENTRY_POINTS)
def test_module_imports_first(module):
    proc = subprocess.run(
        [sys.executable, "-c", f"import {module}"], capture_output=True, text=True, timeout=120, cwd=ROOT
    )
    assert proc.returncode == 0, proc.stderr[-800:]


def test_state_is_one_object_under_both_paths():
    from ai.schemas.state import AgentState as a
    from ai.workflow.state import AgentState as b

    assert a is b
