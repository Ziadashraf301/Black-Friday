"""
Adversarial Safety & Prompt Injection Guardrail Engine.
Applies SOLID design principles (Strategy, Chain of Responsibility, Facade).

Orchestrates:
  - Tier 0: Rapid regex pre-filter (<0.05ms) for instant rejection of known attacks.
  - Tier 1: Jev API (TypeSafe AI) System-1 calibrated Noul guardrail.
"""
from typing import Optional
from ai.schemas import SafetyEvaluationResult
from ai.guardrails.base import BaseSafetyFilter, STANDARD_REFUSAL_MESSAGE
from ai.guardrails.regex_filter import RegexPreFilter
from ai.guardrails.jev_filter import JevSafetyFilter
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
    ):
        self._tier0 = tier0_filter or RegexPreFilter()
        self._tier1 = tier1_filter or JevSafetyFilter()
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
