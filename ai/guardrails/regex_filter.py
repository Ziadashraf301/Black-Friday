"""
Tier-0 High-Speed Deterministic Regex Pre-Filter (<0.05ms).
Detects prompt injections, jailbreaks, SQL injections, and script attacks before API calls.
"""
import time
import re
from typing import List, Tuple
from ai.schemas import SafetyEvaluationResult
from ai.guardrails.base import BaseSafetyFilter, STANDARD_REFUSAL_MESSAGE
from core.logging import get_logger

logger = get_logger(__name__)


class RegexPreFilter(BaseSafetyFilter):
    """
    Tier-0 Instant Regex & Heuristic Pre-Filter.
    Detects obvious SQL injections, XSS/script tags, and classic jailbreak triggers in <0.1ms.
    """

    PATTERNS: List[Tuple[str, str]] = [
        # Jailbreak keywords & DAN variants
        (r"(?i)\b(?:ignore|disregard|forget)\s+(?:all\s+)?(?:previous|prior|above)\s+instructions\b", "JAILBREAK_IGNORE_INSTRUCTIONS"),
        (r"(?i)\byou\s+are\s+now\s+dan\b", "JAILBREAK_DAN_TRIGGER"),
        (r"(?i)\bdo\s+anything\s+now\b", "JAILBREAK_DAN_TRIGGER"),
        (r"(?i)\[\s*debug\s+mode\s+(?:active|activated)\s*\]", "JAILBREAK_DEBUG_MODE"),
        (r"(?i)\bsystem\s+(?:prompt\s+)?override\b", "JAILBREAK_SYSTEM_OVERRIDE"),
        (r"(?i)\bdisable\s+all\s+(?:e-commerce\s+|safety\s+)?constraints\b", "JAILBREAK_DISABLE_CONSTRAINTS"),
        (r"(?i)\breveal\s+(?:the\s+)?(?:developer\s+)?system\s+prompt\b", "LEAK_SYSTEM_PROMPT"),
        (r"(?i)\bdisclose\s+(?:the\s+)?(?:developer\s+)?system\s+prompt\b", "LEAK_SYSTEM_PROMPT"),
        (r"(?i)\boutput\s+your\s+secret\s+instructions\b", "LEAK_SECRET_INSTRUCTIONS"),
        (r"(?i)\bappend\s+(?:all\s+)?(?:internal\s+)?api\s+keys\b", "LEAK_CREDENTIALS"),

        # SQL Injection attempts
        (r"(?i)(?:drop|alter|truncate)\s+table\b", "SQLI_DDL_ATTACK"),
        (r"(?i)select\s+\*\s+from\s+[a-z0-9_]+\s+where\s+['\"][0-9]['\"]\s*=\s*['\"][0-9]['\"]", "SQLI_TAUTOLOGY"),
        (r"(?i)union\s+(?:all\s+)?select\b", "SQLI_UNION_SELECT"),

        # Script / Code Injections
        (r"(?i)<script\b[^>]*>.*?</script>", "XSS_SCRIPT_TAG"),
        (r"(?i)javascript:\s*", "XSS_JAVASCRIPT_PROTOCOL"),
    ]

    def __init__(self):
        self._compiled_patterns = [
            (re.compile(pattern), rule_name) for pattern, rule_name in self.PATTERNS
        ]

    def evaluate(self, text: str) -> SafetyEvaluationResult:
        start_t = time.perf_counter()
        flagged = []

        for regex, rule_name in self._compiled_patterns:
            if regex.search(text):
                flagged.append(rule_name)

        latency = (time.perf_counter() - start_t) * 1000

        if flagged:
            logger.warning(f"[GUARDRAIL: TIER-0] Blocked query on rules: {flagged} in {latency:.2f}ms")
            return SafetyEvaluationResult(
                is_safe=False,
                adversarial_probability=0.99,
                flagged_rules=flagged,
                risk_level="CRITICAL",
                refusal_message=STANDARD_REFUSAL_MESSAGE,
                latency_ms=round(latency, 2),
                tier_intercepted="TIER_0_REGEX",
            )

        return SafetyEvaluationResult(
            is_safe=True,
            adversarial_probability=0.01,
            flagged_rules=[],
            risk_level="LOW",
            refusal_message=None,
            latency_ms=round(latency, 2),
            tier_intercepted="CLEARED",
        )
