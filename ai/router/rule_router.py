"""
Deterministic Local Heuristic Router Strategy (Zero-latency CPU fallback).
"""
from typing import Optional, Dict, Any
import time

from ai.schemas import RoutingDecision, IntentType, ExtractedEntities
from ai.router.base import BaseRouterStrategy
from ai.extractor.base import BaseEntityExtractor
from ai.extractor.regex_extractor import RegexEntityExtractor
from ai.classifier.intent_classifier import IntentClassifier
from ai.guardrails.base import BaseSafetyFilter
from ai.guardrails.regex_filter import RegexPreFilter
from ai.prompts.guardrail_prompts import STANDARD_REFUSAL_MESSAGE


class FastRuleRouter(BaseRouterStrategy):
    """
    Deterministic Local Strategy: Zero-latency, CPU-only heuristic rule engine.
    Ensures 100% offline reproducibility, instant testing (<1ms), and zero API downtime.
    """

    def __init__(
        self,
        entity_extractor: Optional[BaseEntityExtractor] = None,
        safety_filter: Optional[BaseSafetyFilter] = None,
    ):
        self._extractor = entity_extractor or RegexEntityExtractor()
        self._safety = safety_filter or RegexPreFilter()

    def route(self, query: str, context: Optional[Dict[str, Any]] = None) -> RoutingDecision:
        start_t = time.perf_counter()

        # Step 1: Instant local safety validation via Tier-0 Regex (<0.1ms)
        safety_res = self._safety.evaluate(query)
        if not safety_res.is_safe:
            latency = (time.perf_counter() - start_t) * 1000
            return RoutingDecision(
                query=query,
                intent=IntentType.ADVERSARIAL_BLOCKED,
                confidence=safety_res.adversarial_probability,
                is_safe=False,
                adversarial_prob=safety_res.adversarial_probability,
                entities=ExtractedEntities(extraction_strategy="none"),
                steering_response=safety_res.refusal_message or STANDARD_REFUSAL_MESSAGE,
                strategy_used="FastRuleRouter",
                prompt_version="local_rules_v1",
                latency_ms=round(latency, 2),
                metadata={"flagged_rules": safety_res.flagged_rules},
            )

        # Step 2: Multi-intent classification by rules
        multi_intents = IntentClassifier.classify_multi_by_rules(query)
        primary_intent = multi_intents[0]
        conf = 0.90 if len(multi_intents) > 1 else 0.85

        # Step 3: Entity extraction
        entities = self._extractor.extract(query)
        
        # Partition entities per intent branch
        decomposed_entities: Dict[str, ExtractedEntities] = {}
        for it in multi_intents:
            decomposed_entities[it.value] = entities

        # Step 4: Steering for Out-of-Domain
        steering = None
        if primary_intent == IntentType.OUT_OF_DOMAIN:
            steering = IntentClassifier.get_steering_response(query)

        latency = (time.perf_counter() - start_t) * 1000

        return RoutingDecision(
            query=query,
            intent=primary_intent,
            target_intents=multi_intents,
            confidence=conf,
            is_safe=True,
            adversarial_prob=0.01,
            entities=entities,
            decomposed_entities=decomposed_entities,
            steering_response=steering,
            strategy_used="FastRuleRouter",
            prompt_version="local_rules_v1",
            latency_ms=round(latency, 2),
            metadata={"rule_match": True, "multi_intent_count": len(multi_intents)},
        )


__all__ = ["FastRuleRouter"]
