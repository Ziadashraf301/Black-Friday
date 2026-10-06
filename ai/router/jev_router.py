"""
Primary Production Strategy: TypeSafe AI Jev System-1 API Router.
Executes single-pass parallel questions (Noul for safety + Choice for intent)
delivering calibrated probabilities with zero autoregressive generation hallucination.
"""
from typing import Optional, Dict, Any, List
import time

from ai.schemas import RoutingDecision, IntentType, ExtractedEntities
from ai.router.base import BaseRouterStrategy
from ai.router.rule_router import FastRuleRouter
from ai.extractor.base import BaseEntityExtractor
from ai.extractor.regex_extractor import RegexEntityExtractor
from ai.classifier.intent_classifier import IntentClassifier
from ai.prompts.registry import PromptRegistry
from ai.prompts.guardrail_prompts import STANDARD_REFUSAL_MESSAGE
from ai.guardrails.safety_engine import safety_engine
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class JevApiRouter(BaseRouterStrategy):
    """
    Primary Production Strategy: TypeSafe AI Jev System-1 API.
    Executes single-pass parallel questions (Noul for safety + Choice for intent)
    delivering calibrated probabilities with zero autoregressive generation hallucination.
    """

    def __init__(
        self,
        api_key: Optional[str] = None,
        prompt_version: str = "v1.0.0",
        adversarial_threshold: float = 0.80,
        entity_extractor: Optional[BaseEntityExtractor] = None,
    ):
        self._api_key = api_key or getattr(settings, "TYPESAFE_API_KEY", None)
        self._prompt_version = prompt_version
        self._threshold = adversarial_threshold
        self._extractor = entity_extractor or RegexEntityExtractor()
        self._client = None

        if self._api_key:
            try:
                from typesafe_sdk import TypeSafeClient
                self._client = TypeSafeClient(api_key=self._api_key)
                logger.info(f"[ROUTER: JEV] TypeSafeClient initialized with prompt version {self._prompt_version}")
            except Exception as e:
                logger.warning(f"[ROUTER: JEV] Failed to initialize TypeSafeClient: {e}")

    @property
    def is_available(self) -> bool:
        return self._client is not None

    def route(self, query: str, context: Optional[Dict[str, Any]] = None) -> RoutingDecision:
        start_t = time.perf_counter()

        # Step 0: Instant Tier-0 Regex Safety Pre-Filter (<1ms)
        t0_safety = safety_engine.check_safety(query)
        if not t0_safety.is_safe:
            latency = (time.perf_counter() - start_t) * 1000
            return RoutingDecision(
                query=query,
                intent=IntentType.ADVERSARIAL_BLOCKED,
                confidence=t0_safety.adversarial_probability,
                is_safe=False,
                adversarial_prob=t0_safety.adversarial_probability,
                entities=ExtractedEntities(extraction_strategy="none"),
                steering_response=t0_safety.refusal_message or STANDARD_REFUSAL_MESSAGE,
                strategy_used="JevApiRouter (Tier-0 Intercept)",
                prompt_version=self._prompt_version,
                latency_ms=round(latency, 2),
                metadata={"tier": t0_safety.tier_intercepted, "flagged": t0_safety.flagged_rules},
            )

        # Fallback to local rule router if Jev client is not configured
        if not self._client:
            logger.info("[ROUTER: JEV] Client unavailable. Delegating to FastRuleRouter fallback.")
            return FastRuleRouter(entity_extractor=self._extractor).route(query, context)

        # Step 1: Query Jev System-1 API using versioned prompt spec
        try:
            from typesafe_sdk import Noul, Choice
            prompt_spec = PromptRegistry.get(self._prompt_version)
            q_adv = prompt_spec.questions["is_adversarial"]
            q_intent = prompt_spec.questions["intent"]

            response = self._client.system_one(
                state=query,
                questions={
                    "is_adversarial": Noul(instructions=q_adv.instructions),
                    "intent": Choice(
                        instructions=q_intent.instructions,
                        criteria=q_intent.criteria,
                    ),
                },
            )

            latency = (time.perf_counter() - start_t) * 1000
            adv_prob = float(response.nouls["is_adversarial"].noul)
            raw_intent = response.choices["intent"].choice if "intent" in response.choices else None

            # Check if Jev flagged query as an adversarial attack
            if adv_prob >= self._threshold:
                logger.warning(f"[ROUTER: JEV] Adversarial attack detected (P={adv_prob:.4f})")
                return RoutingDecision(
                    query=query,
                    intent=IntentType.ADVERSARIAL_BLOCKED,
                    target_intents=[IntentType.ADVERSARIAL_BLOCKED],
                    confidence=adv_prob,
                    is_safe=False,
                    adversarial_prob=adv_prob,
                    entities=ExtractedEntities(extraction_strategy="none"),
                    steering_response=STANDARD_REFUSAL_MESSAGE,
                    strategy_used="JevApiRouter",
                    prompt_version=self._prompt_version,
                    latency_ms=round(latency, 2),
                    metadata={"tier": "TIER_1_JEV", "raw_intent": raw_intent},
                )

            # Step 2: Resolve Multi-Intents from Parallel Nouls or Choice
            detected_intents: List[IntentType] = []

            # Check parallel Nouls if present in response
            noul_to_intent = {
                "needs_product_details": IntentType.PRODUCT_DETAILS,
                "needs_product_search": IntentType.PRODUCT_SEARCH,
                "needs_bundle_pairing": IntentType.BUNDLE_RECOMMENDATIONS,
                "needs_cart_action": IntentType.CART_ACTIONS,
                "needs_order_support": IntentType.ORDER_SUPPORT,
                "needs_deals_promo": IntentType.DEALS_PROMOTIONS,
            }
            for noul_key, it in noul_to_intent.items():
                if noul_key in response.nouls and float(response.nouls[noul_key].noul) >= 0.50:
                    detected_intents.append(it)

            # Fallback to single choice or rule-based multi-intent if no Nouls triggered
            if raw_intent:
                resolved_choice = IntentClassifier.resolve_intent_type(raw_intent)
                if resolved_choice not in detected_intents:
                    detected_intents.insert(0, resolved_choice)

            # Supplement with rule-based multi-intent detection
            rule_multi = IntentClassifier.classify_multi_by_rules(query)
            for r_it in rule_multi:
                if r_it not in detected_intents and r_it != IntentType.ADVERSARIAL_BLOCKED:
                    detected_intents.append(r_it)

            primary_intent = detected_intents[0] if detected_intents else IntentType.PRODUCT_SEARCH
            entities = self._extractor.extract(query)

            # Partition entities per intent branch
            decomposed_entities: Dict[str, ExtractedEntities] = {}
            for it in detected_intents:
                decomposed_entities[it.value] = entities

            # Step 3: Domain Steering if Out-of-Domain
            steering_resp = None
            if primary_intent == IntentType.OUT_OF_DOMAIN:
                steering_resp = IntentClassifier.get_steering_response(query)

            return RoutingDecision(
                query=query,
                intent=primary_intent,
                target_intents=detected_intents,
                confidence=round(1.0 - adv_prob, 4),
                is_safe=True,
                adversarial_prob=adv_prob,
                entities=entities,
                decomposed_entities=decomposed_entities,
                steering_response=steering_resp,
                strategy_used="JevApiRouter",
                prompt_version=self._prompt_version,
                latency_ms=round(latency, 2),
                metadata={"raw_choice": raw_intent, "multi_intent_count": len(detected_intents)},
            )

        except Exception as e:
            latency = (time.perf_counter() - start_t) * 1000
            logger.error(f"[ROUTER: JEV] API call failed: {e}. Falling back to FastRuleRouter.")
            fallback = FastRuleRouter(entity_extractor=self._extractor).route(query, context)
            fallback.strategy_used = "JevApiRouter (Fallback)"
            fallback.latency_ms = round(latency + fallback.latency_ms, 2)
            return fallback


__all__ = ["JevApiRouter"]
