"""INF-21: the API image must ship boto3, and the tracking setup warns when it is missing."""

import importlib.util
import re
from pathlib import Path

from core.tracking import client

ROOT = Path(__file__).resolve().parents[3]


def test_api_requirements_include_boto3():
    lines = (ROOT / "requirements.txt").read_text(encoding="utf-8").splitlines()
    assert any(re.match(r"boto3\b", line) for line in lines)


def test_setup_warns_without_boto3(monkeypatch):
    warnings = []
    monkeypatch.setattr(importlib.util, "find_spec", lambda name: None if name == "boto3" else object())
    monkeypatch.setattr(client.logger, "warning", lambda msg, *a, **k: warnings.append(msg))
    client.setup_tracking_environment()
    assert any("boto3 is not installed" in w for w in warnings)


def test_setup_is_quiet_with_boto3(monkeypatch):
    warnings = []
    monkeypatch.setattr(client, "has_s3_client", lambda: True)
    monkeypatch.setattr(client.logger, "warning", lambda msg, *a, **k: warnings.append(msg))
    client.setup_tracking_environment()
    assert warnings == []
