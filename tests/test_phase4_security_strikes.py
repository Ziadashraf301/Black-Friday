"""
Unit and Integration Tests for Automated Adversarial Strike Tracker & Ban Gateway (Phase 4 - Task P4-07).
Validates:
  1. Adversarial strike accumulation (3-strike limit).
  2. 24-hour lockout activation upon 3 strikes.
  3. Fast-path guardrail evaluation rejection for locked-out shoppers.
  4. FastAPI Gateway Middleware returning 403 Forbidden with SECURITY_STRIKE_LOCKOUT.
"""
import pytest
from fastapi.testclient import TestClient
from ai.guardrails.strike_tracker import strike_tracker
from ai.services.guardrail_service import guardrail_service
from apps.api.main import app


class TestSecurityStrikeTracker:
    """Verifies strike counter, automatic lockout, and gateway HTTP 403 enforcement."""

    @pytest.fixture(autouse=True)
    def setup_user(self):
        self.attacker_id = "test_attacker_user_99"
        strike_tracker.reset_strikes(self.attacker_id)
        yield
        strike_tracker.reset_strikes(self.attacker_id)

    def test_strike_accumulation_and_lockout(self):
        from core.cache.redis_client import cache_manager

        # Initial state: clean
        assert strike_tracker.is_banned(self.attacker_id) is False

        # Strike 1: Malicious query
        s1 = strike_tracker.record_strike(self.attacker_id)
        assert s1 == 1
        assert strike_tracker.is_banned(self.attacker_id) is False
        if cache_manager.is_available and cache_manager.client is not None:
            assert cache_manager.client.get(f"security:strikes:{self.attacker_id}") == "1"

        # Strike 2: Second attempt
        s2 = strike_tracker.record_strike(self.attacker_id)
        assert s2 == 2
        assert strike_tracker.is_banned(self.attacker_id) is False
        if cache_manager.is_available and cache_manager.client is not None:
            assert cache_manager.client.get(f"security:strikes:{self.attacker_id}") == "2"

        # Strike 3: Reaches threshold -> 24h ban triggered!
        s3 = strike_tracker.record_strike(self.attacker_id)
        assert s3 == 3
        assert strike_tracker.is_banned(self.attacker_id) is True
        if cache_manager.is_available and cache_manager.client is not None:
            assert cache_manager.client.get(f"security:strikes:{self.attacker_id}") == "3"
            assert cache_manager.client.get(f"security:banned:{self.attacker_id}") == "BANNED_FOR_24H"
            assert cache_manager.client.ttl(f"security:banned:{self.attacker_id}") > 0

    def test_guardrail_service_lockout_fast_path(self):
        # Manually ban user
        strike_tracker.record_strike(self.attacker_id)
        strike_tracker.record_strike(self.attacker_id)
        strike_tracker.record_strike(self.attacker_id)
        assert strike_tracker.is_banned(self.attacker_id) is True

        # Even an innocent query must be immediately rejected with sub-millisecond lockout
        res = guardrail_service.evaluate_query(
            query="Hello do you have silk shirts?",
            user_id=self.attacker_id,
        )
        assert res.is_safe is False
        assert res.adversarial_probability == 1.0
        assert res.strategy_used == "StrikeTrackerGateway"
        assert "Access revoked" in res.steering_response

    def test_fastapi_gateway_middleware_403(self):
        client = TestClient(app)

        # 1. Clean user can access shopper endpoint
        resp_clean = client.get("/shopper/curated-catalog", headers={"X-User-ID": "innocent_shopper"})
        assert resp_clean.status_code == 200

        # 2. Ban test attacker
        strike_tracker.record_strike(self.attacker_id)
        strike_tracker.record_strike(self.attacker_id)
        strike_tracker.record_strike(self.attacker_id)
        assert strike_tracker.is_banned(self.attacker_id) is True

        # 3. Request from banned user must receive HTTP 403 Forbidden immediately
        resp_banned = client.get("/shopper/curated-catalog", headers={"X-User-ID": self.attacker_id})
        assert resp_banned.status_code == 403
        data = resp_banned.json()
        assert data.get("error_code") == "SECURITY_STRIKE_LOCKOUT"
        assert "Lockout expires in 24 hours" in data.get("detail", "")
