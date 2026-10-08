"""
Architecture and Layering Boundary Enforcement Tests.
Uses AST inspection to statically verify architectural dependency boundaries:
- core/ imports nothing from ml/, ai/, apps/, evaluation/
- ml/ and ai/ import core/ only (never apps, never each other's pipelines)
- apps/ may import core/, ai/, ml/, but never evaluation/
- evaluation/ may import core/, ml/, ai/, but nothing imports evaluation/ (except allow-listed training pipelines)
- mlflow is configured and imported ONLY in core/tracking/
"""
import ast
from pathlib import Path
from typing import List, Tuple

PROJECT_ROOT = Path(__file__).resolve().parent.parent

EXCLUDED_DIRS = {
    ".git",
    ".venv",
    "legacy_project",
    ".web",
    "tests",
    "__pycache__",
    ".pytest_cache",
    ".ruff_cache",
    "node_modules",
}

# Allow-list for imports that cannot be decoupled without changing behavior.
# All previous allow-listed entries from WP0 were resolved in WP4 by relocating
# metric computations to ml/models/metrics.py.
ALLOW_LISTED_VIOLATIONS = set()


def scan_architectural_imports() -> List[str]:
    """Scans all Python source files in the project for dependency rule violations."""
    violations: List[str] = []

    for py_path in PROJECT_ROOT.rglob("*.py"):
        rel_path = py_path.relative_to(PROJECT_ROOT)
        parts = rel_path.parts

        # Skip excluded directories
        if any(part in EXCLUDED_DIRS for part in parts):
            continue

        top_dir = parts[0]
        sub_dir = parts[1] if len(parts) > 1 else ""
        norm_path = rel_path.as_posix()

        try:
            with open(py_path, "r", encoding="utf-8") as f:
                tree = ast.parse(f.read(), filename=str(py_path))
        except Exception as e:
            violations.append(f"{norm_path}:0: AST ParseError: {e}")
            continue

        for node in ast.walk(tree):
            imported_modules: List[Tuple[int, str]] = []
            if isinstance(node, ast.Import):
                for alias in node.names:
                    imported_modules.append((node.lineno, alias.name))
            elif isinstance(node, ast.ImportFrom):
                if node.module:
                    imported_modules.append((node.lineno, node.module))

            for lineno, mod_name in imported_modules:
                top_module = mod_name.split(".")[0]

                # Check Rule: MLflow imported strictly inside core/tracking/
                if top_module == "mlflow":
                    if not (top_dir == "core" and sub_dir == "tracking"):
                        violations.append(
                            f"{norm_path}:{lineno}: Forbidden direct import of 'mlflow' outside core/tracking/"
                        )

                # Check Rule: core/ imports nothing from ml, ai, apps, evaluation
                if top_dir == "core":
                    if top_module in ("ml", "ai", "apps", "evaluation"):
                        violations.append(
                            f"{norm_path}:{lineno}: Forbidden import of '{mod_name}' inside core/"
                        )

                # Check Rule: ml/ imports core only (never apps, ai, evaluation)
                if top_dir == "ml":
                    if top_module in ("apps", "ai"):
                        violations.append(
                            f"{norm_path}:{lineno}: Forbidden import of presentation/agent '{mod_name}' inside ml/"
                        )
                    elif top_module == "evaluation":
                        if (norm_path, mod_name) not in ALLOW_LISTED_VIOLATIONS:
                            violations.append(
                                f"{norm_path}:{lineno}: Forbidden import of '{mod_name}' inside ml/"
                            )

                # Check Rule: ai/ imports core only (never apps, ml, evaluation)
                if top_dir == "ai":
                    if top_module in ("apps", "ml", "evaluation"):
                        violations.append(
                            f"{norm_path}:{lineno}: Forbidden cross-domain import of '{mod_name}' inside ai/"
                        )

                # Check Rule: apps/ imports core, ai, ml (never evaluation)
                if top_dir == "apps":
                    if top_module == "evaluation":
                        violations.append(
                            f"{norm_path}:{lineno}: Forbidden import of '{mod_name}' inside apps/"
                        )

    return violations


def test_dependency_rules_and_mlflow_isolation():
    """Validates that all architectural layering boundaries and MLflow isolation pass."""
    violations = scan_architectural_imports()
    if violations:
        formatted = "\n  - ".join(violations)
        assert False, f"Architectural dependency violations detected ({len(violations)}):\n  - {formatted}"
