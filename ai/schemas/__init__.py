"""
Centralized AI Domain Schemas Registry.
Single source of truth for all data contracts across AI services, routers, guardrails, and workflows.
"""

from ai.schemas.guardrails import (
    GuardrailResponse,
    SafetyEvaluationResult,
)
from ai.schemas.router import (
    ExtractedEntities,
    IntentType,
    PromptVersion,
    QuestionSpec,
    RoutingDecision,
)
from ai.schemas.workflow import (
    AgentResponsePayload,
    BundleItem,
    CartItem,
    CartState,
    OrderStatus,
    ProductDetails,
    ProductSearchResult,
    UserProfile,
)

__all__ = [
    # Router & Entities
    "IntentType",
    "ExtractedEntities",
    "RoutingDecision",
    "QuestionSpec",
    "PromptVersion",
    # Guardrails
    "SafetyEvaluationResult",
    "GuardrailResponse",
    # Workflow & Tools
    "ProductSearchResult",
    "ProductDetails",
    "BundleItem",
    "CartItem",
    "CartState",
    "OrderStatus",
    "UserProfile",
    "AgentResponsePayload",
]
