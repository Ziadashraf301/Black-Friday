"""The pytest markers used by the test folders are registered in pyproject.toml."""
from pathlib import Path

PYPROJECT = Path(__file__).resolve().parents[2] / "pyproject.toml"


def test_pyproject_registers_benchmark_marker():
    """Confirms pyproject.toml contains the benchmark marker declaration."""
    content = PYPROJECT.read_text(encoding="utf-8")
    assert "markers =" in content
    assert "benchmark:" in content
