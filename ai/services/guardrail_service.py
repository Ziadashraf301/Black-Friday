"""
Guardrail & System-1 Router Service Layer (Facade Pattern).
Orchestrates safety pre-screening, System-1 routing, entity extraction,
and audit logging for FastAPI routers and client applications.
"""
from typing import Optional, Dict, Any
from datetime import datetime, timezone
from pydantic import BaseModel, Field
from ai.router import BaseRouterStrategy, RouterFactory, intent_router
from ai.schemas import GuardrailResponse, RoutingDecision, IntentType, ExtractedEntities
from core.logging import get_logger

logger = get_logger(__name__)


class GuardrailService:
    """
    Facade orchestrator coordinating System-1 safety checks,
    intent classification, entity extraction, and audit trails.
    """

    def __init__(self, router: Optional[BaseRouterStrategy] = None):
        self._router = router or intent_router
        logger.info(f"[GUARDRAIL-SERVICE] Initialized with active router: {self._router.__class__.__name__}")

    @property
    def router(self) -> BaseRouterStrategy:
        return self._router

    def set_router(self, router: BaseRouterStrategy) -> None:
        """Allows runtime swapping of router strategy (e.g. for offline testing)."""
        self._router = router
        logger.info(f"[GUARDRAIL-SERVICE] Router strategy swapped to {router.__class__.__name__}")

    def evaluate_query(
        self,
        query: str,
        user_id: Optional[str] = None,
        session_id: Optional[str] = None,
        context: Optional[Dict[str, Any]] = None,
    ) -> GuardrailResponse:
        """
        Evaluates user query through System-1 pipeline.
        Returns unified GuardrailResponse.
        """
        # 0. Check strike tracker lockout (Phase 4 - Task P4-07)
        from ai.guardrails.strike_tracker import strike_tracker
        if user_id and strike_tracker.is_banned(user_id):
            logger.warning(f"[SECURITY: LOCKOUT] Rejected request from banned user={user_id}")
            return GuardrailResponse(
                query=query,
                intent=IntentType.ADVERSARIAL_BLOCKED.value,
                target_intents=[IntentType.ADVERSARIAL_BLOCKED.value],
                is_safe=False,
                confidence=1.0,
                adversarial_probability=1.0,
                entities=ExtractedEntities(extraction_strategy="none"),
                decomposed_entities={IntentType.ADVERSARIAL_BLOCKED.value: ExtractedEntities().model_dump()},
                steering_response="Access revoked due to repeated security policy violations. Lockout expires in 24 hours.",
                strategy_used="StrikeTrackerGateway",
                prompt_version="v2.0_multi_intent",
                latency_ms=0.1,
                user_id=user_id,
                session_id=session_id,
                timestamp=datetime.now(timezone.utc).isoformat(),
            )

        logger.info(f"[GUARDRAIL-SERVICE] Evaluating query for user={user_id}: '{query[:60]}...'")

        decision: RoutingDecision = self._router.route(query, context=context)

        # Record strike on adversarial violation
        if not decision.is_safe and user_id:
            strikes = strike_tracker.record_strike(user_id)
            logger.warning(
                f"[SECURITY-AUDIT] Blocked adversarial query from user={user_id} "
                f"(Strike {strikes}/{strike_tracker.MAX_STRIKES}): "
                f"Prob={decision.adversarial_prob:.4f}, Tier={decision.strategy_used}"
            )
        else:
            logger.debug(
                f"[GUARDRAIL-SERVICE] Safe query routed: Intent={decision.intent.value}, "
                f"Latency={decision.latency_ms}ms"
            )
        target_intents_list = [it.value for it in decision.target_intents] if decision.target_intents else [decision.intent.value]
        decomposed_entities_dict = {
            k: v.model_dump() for k, v in decision.decomposed_entities.items()
        } if decision.decomposed_entities else {decision.intent.value: decision.entities.model_dump()}

        response = GuardrailResponse(
            query=decision.query,
            intent=decision.intent.value,
            target_intents=target_intents_list,
            is_safe=decision.is_safe,
            confidence=decision.confidence,
            adversarial_probability=decision.adversarial_prob,
            entities=decision.entities,
            decomposed_entities=decomposed_entities_dict,
            steering_response=decision.steering_response,
            strategy_used=decision.strategy_used,
            prompt_version=decision.prompt_version,
            latency_ms=decision.latency_ms,
            user_id=user_id,
            session_id=session_id,
            timestamp=datetime.now(timezone.utc).isoformat(),
        )

        return response


# Singleton service export
guardrail_service = GuardrailService()
