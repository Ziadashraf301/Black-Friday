"""
Regression Tests for Fix 9.6 (Hot Reload of Store Policies via File mtime).
Validates:
1. PolicyKnowledgeBase reloads store_policies.json when file mtime changes.
2. Updates in a temporary file reflect immediately in get_policy without restarting or re-instantiating.
"""
import json
import time
from pathlib import Path
import pytest

from ai.tools.policy_kb import PolicyKnowledgeBase


def test_policy_kb_hot_reloads_on_mtime_change(tmp_path):
    """Verifies that changing policy JSON file content in a temp dir is picked up without restart."""
    policy_file = tmp_path / "store_policies.json"

    initial_data = {
        "policies": {
            "returns": {
                "topic": "returns",
                "title": "Initial 30-Day Return Policy",
                "summary": "Returns allowed for 30 days.",
                "keywords": ["return", "refund"],
            }
        }
    }
    policy_file.write_text(json.dumps(initial_data), encoding="utf-8")

    # Instantiate KB pointing to temp file
    kb = PolicyKnowledgeBase(policy_file_path=policy_file)
    p1 = kb.get_policy("returns")
    assert p1["title"] == "Initial 30-Day Return Policy"
    assert p1["summary"] == "Returns allowed for 30 days."

    # Wait a small delay to ensure mtime changes
    time.sleep(0.05)

    # Modify policy file
    updated_data = {
        "policies": {
            "returns": {
                "topic": "returns",
                "title": "Updated Extended 90-Day Holiday Returns",
                "summary": "Free holiday returns extended through January 31, 2027.",
                "keywords": ["return", "refund", "exchange"],
            },
            "cyber_deals": {
                "topic": "cyber_deals",
                "title": "Cyber Monday Doorbuster Rules",
                "summary": "50% off sitewide on Cyber Monday.",
                "keywords": ["cyber", "doorbuster"],
            },
        }
    }
    policy_file.write_text(json.dumps(updated_data), encoding="utf-8")

    # Query the existing instance without restart
    p2 = kb.get_policy("returns")
    assert p2["title"] == "Updated Extended 90-Day Holiday Returns"
    assert p2["summary"] == "Free holiday returns extended through January 31, 2027."

    # New topic should also be accessible
    p_cyber = kb.get_policy("cyber_deals")
    assert p_cyber["title"] == "Cyber Monday Doorbuster Rules"
