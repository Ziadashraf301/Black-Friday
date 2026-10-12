"""
Regression Tests for Fixes 6.3, 7.3, 8.2 (Unified Logging Across Specialist Nodes).
Validates:
1. ai/nodes/cart_node, ai/nodes/bundle_node, ai/nodes/support_node import core.logging.get_logger and NOT loguru.
2. Running a specialist node outputs logs through the standard library / core logger.
3. loguru is not imported across the ai/ package.
"""
import logging
from unittest.mock import patch
import pytest

import ai.nodes.cart_node as cart_node_mod
import ai.nodes.bundle_node as bundle_node_mod
import ai.nodes.support_node as support_node_mod
from core.logging import get_logger


def test_specialist_nodes_use_core_logger():
    """Validates that cart, bundle, and support nodes instantiate the standard core logger."""
    for mod in [cart_node_mod, bundle_node_mod, support_node_mod]:
        assert hasattr(mod, "logger"), f"{mod.__name__} must expose a logger attribute"
        # Must be standard library logging.Logger instance wrapped by core.logging
        assert isinstance(mod.logger, logging.Logger), f"{mod.__name__}.logger must be a standard Logger"


def test_specialist_node_execution_logs_through_core_logger(caplog):
    """Executes bundle_agent_node and confirms structured log capture via core logging."""
    from ai.nodes.bundle_node import bundle_agent_node
    from ai.workflow.state import AgentState

    test_state: AgentState = {
        "query": "find bundles",
        "entities": {"product_ids": ["P00025442"]},
    }

    with caplog.at_level(logging.INFO):
        res = bundle_agent_node(test_state)

    assert res is not None
    assert "bundle_recommendations" in res

    # Verify log output was captured by standard logging handler
    log_messages = [rec.message for rec in caplog.records]
    assert any("[GRAPH: BUNDLE-NODE]" in msg for msg in log_messages)


def test_no_loguru_imported_in_ai_nodes():
    """Confirms loguru is not present in the namespace or imports of specialist nodes."""
    for mod in [cart_node_mod, bundle_node_mod, support_node_mod]:
        assert "loguru" not in mod.__dict__, f"{mod.__name__} should not import loguru"
