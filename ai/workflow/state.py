"""
LangGraph StateGraph State Definition (Phase 3).
Encapsulates message streams, extracted entities, routing decisions,
shopping cart state, and intermediate specialist tool outputs.
"""
from typing import TypedDict, Annotated, List, Optional, Dict, Any
import operator
from langgraph.graph.message import add_messages
from langchain_core.messages import BaseMessage


def merge_dicts(a: Optional[Dict[str, Any]], b: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    """Safe dictionary reducer for parallel node state updates."""
    res = dict(a or {})
    res.update(b or {})
    return res


def last_str_reducer(a: Optional[str], b: Optional[str]) -> Optional[str]:
    """Safe string reducer for parallel node state updates."""
    return b if b is not None else a


class AgentState(TypedDict, total=False):
    """
    Central execution state flowing through the LangGraph StateGraph.
    Supports immutable updates and Redis checkpointer serialization.
    """
    # Conversational message history with append reducer
    messages: Annotated[List[BaseMessage], add_messages]

    # Session & Query Context
    query: str
    user_id: str
    session_id: str

    # Guardrail & System-1 Routing Outputs
    intent: Optional[str]
    target_intents: Optional[List[str]]
    entities: Optional[Dict[str, Any]]
    decomposed_entities: Optional[Dict[str, Any]]
    is_safe: bool
    adversarial_prob: float
    steering_response: Optional[str]
    routing_latency_ms: float

    # User Profile & Shopping Preferences
    user_profile: Optional[Dict[str, Any]]

    # Active Shopping Cart
    cart: Optional[Dict[str, Any]]

    # Tool Execution Artifacts with append reducers for parallel specialist execution
    retrieved_products: Annotated[List[Dict[str, Any]], operator.add]
    product_details: Optional[Dict[str, Any]]
    bundle_recommendations: Annotated[List[Dict[str, Any]], operator.add]
    order_status: Optional[Dict[str, Any]]
    policy_details: Optional[Dict[str, Any]]
    retrieved_docs: Annotated[List[Dict[str, Any]], operator.add]

    # Synthesis & UI Output
    final_response: Optional[str]
    ui_payload: Optional[Dict[str, Any]]
    is_cache_hit: Optional[bool]
    cache_tier: Optional[str]

    # Execution control with parallel-safe reducers
    current_node: Annotated[Optional[str], last_str_reducer]
    error_message: Annotated[Optional[str], last_str_reducer]
    relaxation_level: Annotated[Optional[str], last_str_reducer]
