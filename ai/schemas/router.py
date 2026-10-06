"""
System-1 Router & Entity Extraction Domain Schemas.
"""
from enum import Enum
from typing import Optional, List, Dict, Any
from pydantic import BaseModel, Field


class IntentType(str, Enum):
    """Canonical 8 e-commerce intents handled by the Black Friday AI system."""
    PRODUCT_SEARCH = "PRODUCT_SEARCH"
    PRODUCT_DETAILS = "PRODUCT_DETAILS"
    DEALS_PROMOTIONS = "DEALS_PROMOTIONS"
    BUNDLE_RECOMMENDATIONS = "BUNDLE_RECOMMENDATIONS"
    CART_ACTIONS = "CART_ACTIONS"
    ORDER_SUPPORT = "ORDER_SUPPORT"
    OUT_OF_DOMAIN = "OUT_OF_DOMAIN"
    ADVERSARIAL_BLOCKED = "ADVERSARIAL_BLOCKED"


class ExtractedEntities(BaseModel):
    """Sanitized and validated shopping constraints extracted from user input."""
    category: Optional[str] = Field(None, description="Standardized apparel category")
    categories: List[str] = Field(default_factory=list, description="Extracted categories list")
    max_price: Optional[float] = Field(None, ge=0.0, description="Upper budget constraint in USD")
    size: Optional[str] = Field(None, description="Apparel size (XS, S, M, L, XL, XXL)")
    sizes: List[str] = Field(default_factory=list, description="Extracted sizes list")
    product_id: Optional[str] = Field(None, description="Catalog product ID (e.g. P00025442)")
    product_ids: List[str] = Field(default_factory=list, description="Extracted product IDs list")
    materials: List[str] = Field(default_factory=list, description="Extracted fabric/materials list")
    product_name: Optional[str] = Field(None, description="Product name or title")
    action: Optional[str] = Field(None, description="add, remove, clear, or view")
    quantity: Optional[int] = Field(None, ge=1, description="Item quantity for cart mutations")
    order_id: Optional[str] = Field(None, description="Trackable order number (e.g. ORD-100234)")
    sentiment: Optional[str] = Field(None, description="Customer sentiment tone")
    extraction_strategy: str = Field(default="none", description="Extraction strategy used")


class RoutingDecision(BaseModel):
    """Deterministic routing decision packet emitted by System-1 routers."""
    query: str
    intent: IntentType
    target_intents: List[IntentType] = Field(default_factory=list, description="All detected intents for multi-intent dispatch")
    is_safe: bool = True
    confidence: float = Field(default=1.0, ge=0.0, le=1.0)
    adversarial_prob: float = Field(default=0.0, ge=0.0, le=1.0)
    entities: ExtractedEntities = Field(default_factory=ExtractedEntities)
    decomposed_entities: Dict[str, ExtractedEntities] = Field(default_factory=dict, description="Partitioned entity constraints per intent branch")
    steering_response: Optional[str] = None
    strategy_used: str = "JEV"
    prompt_version: str = "v1.0.0"
    latency_ms: float = 0.0
    metadata: Dict[str, Any] = Field(default_factory=dict)


class QuestionSpec(BaseModel):
    """Specification of a System-1 question for TypeSafe AI."""
    question_type: str = Field(default="noul", description="noul or choice")
    instructions: str = Field(default="", description="Target instruction / question prompt.")
    criteria: Optional[Dict[str, str]] = Field(default=None, description="Criteria dictionary for choice questions.")


class PromptVersion(BaseModel):
    """Immutable specification for a versioned prompt bundle."""
    version: str = Field(..., description="Semantic version string, e.g. v1.0.0")
    description: str = Field(..., description="Summary of prompt changes or purpose.")
    author: str = Field(default="MLOps Team", description="Author or system responsible.")
    created_at: str = Field(default="2026-10-05T10:00:00Z")
    questions: Dict[str, QuestionSpec] = Field(..., description="Map of question_id to QuestionSpec.")

