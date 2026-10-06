"""
Guardrail & Safety Domain Schemas.
Centralized definitions for safety filtering and guardrail service evaluations.
"""
from typing import Optional, List, Dict, Any
from pydantic import BaseModel, Field
from ai.schemas.router import ExtractedEntities


class SafetyEvaluationResult(BaseModel):
    """Pydantic model representing the safety verdict of a user query."""
    is_safe: bool = Field(..., description="True if query is safe to process, False if blocked.")
    adversarial_probability: float = Field(..., ge=0.0, le=1.0, description="Calibrated probability of attack.")
    flagged_rules: List[str] = Field(default_factory=list, description="List of triggered rules or detectors.")
    risk_level: str = Field(default="LOW", description="LOW, MEDIUM, HIGH, or CRITICAL.")
    refusal_message: Optional[str] = Field(default=None, description="Safe refusal text if blocked.")
    latency_ms: float = Field(default=0.0, description="Evaluation latency in milliseconds.")
    tier_intercepted: str = Field(default="CLEARED", description="TIER_0_REGEX, TIER_1_JEV, or CLEARED.")


class GuardrailResponse(BaseModel):
    """Unified service response contract for AI guardrail & router evaluation."""
    query: str
    intent: str
    target_intents: List[str] = Field(default_factory=list, description="All detected intents for multi-intent handling")
    is_safe: bool
    confidence: float
    adversarial_probability: float
    entities: ExtractedEntities
    decomposed_entities: Dict[str, Any] = Field(default_factory=dict, description="Partitioned entities per intent branch")
    steering_response: Optional[str] = None
    strategy_used: str
    prompt_version: str
    latency_ms: float
    user_id: Optional[str] = None
    session_id: Optional[str] = None
    timestamp: str
