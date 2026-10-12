"""
Adversarial Safety & Prompt Injection Guardrail Engine.
Applies SOLID design principles (Strategy, Chain of Responsibility, Facade).

Orchestrates:
  - Tier 0: Rapid regex pre-filter (<0.05ms) for instant rejection of known attacks.
  - Tier 1: Jev API (TypeSafe AI) System-1 calibrated Noul guardrail.
"""

from typing import Optional

from ai.guardrails.base import STANDARD_REFUSAL_MESSAGE, BaseSafetyFilter
from ai.guardrails.jev_filter import JEV_FALLBACK_RULE, JevSafetyFilter
from ai.guardrails.regex_filter import RegexPreFilter
from ai.schemas import SafetyEvaluationResult
from core.logging import get_logger

logger = get_logger(__name__)


class SafetyEngine:
    """
    Facade & Chain of Responsibility orchestrator.
    Passes query through Tier-0 Regex first (<0.05ms), then Tier-1 Jev if clear.
    """

    def __init__(
        self,
        tier0_filter: Optional[BaseSafetyFilter] = None,
        tier1_filter: Optional[BaseSafetyFilter] = None,
        outage_filter: Optional[BaseSafetyFilter] = None,
    ):
        self._tier0 = tier0_filter or RegexPreFilter()
        self._tier1 = tier1_filter or JevSafetyFilter()
        self._outage = outage_filter or RegexPreFilter(strict=True)
        logger.info("[SAFETY-ENGINE] Initialized with Tier-0 Regex and Tier-1 Jev filters.")

    def check_safety(self, text: str) -> SafetyEvaluationResult:
        # Step 1: Run Tier-0 Regex (<0.05ms)
        t0_result = self._tier0.evaluate(text)
        if not t0_result.is_safe:
            return t0_result

        # Step 2: Run Tier-1 Deep Jev Semantic Filter
        t1_result = self._tier1.evaluate(text)
        # Combine latency
        combined_latency = round(t0_result.latency_ms + t1_result.latency_ms, 2)
        t1_result.latency_ms = combined_latency

        # Step 3: Jev was unreachable, so its "safe" is not a judgement. Apply the stricter Tier-0 rule set instead.
        if JEV_FALLBACK_RULE in t1_result.flagged_rules:
            outage_result = self._outage.evaluate(text)
            if not outage_result.is_safe:
                logger.warning(
                    f"[SAFETY-ENGINE] Blocked by strict Tier-0 rules during Jev outage: {outage_result.flagged_rules}"
                )
                outage_result.latency_ms = round(combined_latency + outage_result.latency_ms, 2)
                return outage_result
        return t1_result


# Singleton instance
safety_engine = SafetyEngine()

__all__ = [
    "BaseSafetyFilter",
    "RegexPreFilter",
    "JevSafetyFilter",
    "SafetyEngine",
    "safety_engine",
    "STANDARD_REFUSAL_MESSAGE",
]
