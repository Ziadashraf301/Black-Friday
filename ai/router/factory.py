"""
System-1 Router Factory with Strategy Pattern instantiation.
"""
from typing import Optional
from ai.router.base import BaseRouterStrategy
from ai.router.rule_router import FastRuleRouter
from ai.router.jev_router import JevApiRouter
from ai.extractor.factory import EntityExtractorFactory
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class RouterFactory:
    """Factory creating configured router strategy with dependency injection."""

    @staticmethod
    def create_router(
        strategy: Optional[str] = None,
        api_key: Optional[str] = None,
        prompt_version: str = "v1.0.0",
        extractor_type: str = "regex",
    ) -> BaseRouterStrategy:
        extractor = EntityExtractorFactory.get_extractor(extractor_type)
        key = api_key or getattr(settings, "TYPESAFE_API_KEY", None)

        if strategy == "rule":
            logger.info("[ROUTER-FACTORY] Spawning FastRuleRouter strategy.")
            return FastRuleRouter(entity_extractor=extractor)

        if key:
            logger.info("[ROUTER-FACTORY] Spawning JevApiRouter strategy with active TYPESAFE_API_KEY.")
            return JevApiRouter(
                api_key=key,
                prompt_version=prompt_version,
                entity_extractor=extractor,
            )

        logger.info("[ROUTER-FACTORY] No TYPESAFE_API_KEY detected. Defaulting to FastRuleRouter.")
        return FastRuleRouter(entity_extractor=extractor)


# Default singleton router
intent_router = RouterFactory.create_router()

__all__ = ["RouterFactory", "intent_router"]
