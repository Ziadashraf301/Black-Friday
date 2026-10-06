"""
Centralized AI Domain Schemas Registry.
Single source of truth for all data contracts across AI services, routers, guardrails, and workflows.
"""
from ai.schemas.router import (
    IntentType,
    ExtractedEntities,
    RoutingDecision,
    QuestionSpec,
    PromptVersion,
)
from ai.schemas.guardrails import (
    SafetyEvaluationResult,
    GuardrailResponse,
)
from ai.schemas.workflow import (
    ProductSearchResult,
    ProductDetails,
    BundleItem,
    CartItem,
    CartState,
    OrderStatus,
    UserProfile,
    AgentResponsePayload,
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
