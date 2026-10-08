"""
Regression Tests for Fix 8.5 (JEV Router Latency Assertion Decoupling).
Validates:
1. FastRuleRouter routes queries accurately on unit runs without strict sub-millisecond assertions.
2. Timing checks are marked with the registered pytest 'benchmark' marker.
"""
from pathlib import Path
import pytest
from ai.router.rule_router import FastRuleRouter
from ai.schemas import IntentType, RoutingDecision


def test_router_unit_run_correctness():
    """Confirms FastRuleRouter executes cleanly without brittle sub-millisecond failures."""
    router = FastRuleRouter()
    decision = router.route("Find leather jackets under $100 in size L")
    assert isinstance(decision, RoutingDecision)
    assert decision.intent == IntentType.PRODUCT_SEARCH
    assert decision.is_safe is True
    assert decision.entities.max_price == 100.0


def test_pytest_ini_registers_benchmark_marker():
    """Confirms pytest.ini contains the benchmark marker declaration."""
    ini_path = Path("pytest.ini")
    assert ini_path.exists()
    content = ini_path.read_text(encoding="utf-8")
    assert "markers =" in content
    assert "benchmark:" in content
