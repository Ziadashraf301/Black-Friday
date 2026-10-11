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


class UICard(TypedDict, total=False):
    """Unified UI card schema for catalog, bundle, detail, and promo displays."""
    type: str
    product_id: Optional[str]
    name: Optional[str]
    price: Optional[float]
    original_price: Optional[float]
    bundle_price: Optional[float]
    discount_pct: Optional[str]
    image_url: Optional[str]
    badge: Optional[str]
    materials: Optional[List[str]]
    care_instructions: Optional[str]
    sizes: Optional[List[str]]
    stock_status: Optional[str]


class GroundingCitation(TypedDict, total=False):
    """Grounding citation link linking assistant claims to verified catalog items."""
    product_id: str
    title: str
    url: str
    price: float
    badge: str


class UIPayload(TypedDict, total=False):
    """
    Unified UI Payload schema across specialist nodes, aggregator barrier, and synthesis.
    Consolidates interactive cards, user action chips, and verifiable catalog citations.
    """
    type: Optional[str]
    data: Optional[Dict[str, Any]]
    cards: List[Dict[str, Any]]
    action_chips: List[str]
    citations: List[Dict[str, Any]]
    relaxation_level: Optional[str]


def ui_payload_reducer(
    a: Optional[Dict[str, Any]],
    b: Optional[Dict[str, Any]],
) -> Optional[Dict[str, Any]]:
    """
    Explicit reducer merging UI payload cards, action chips, and citations across specialist nodes.
    Ensures parallel fanout nodes combine their interactive cards without clobbering.
    """
    if a is None and b is None:
        return None
    if a is None:
        return dict(b or {})
    if b is None:
        return dict(a or {})

    merged: Dict[str, Any] = dict(a)
    for k, v in b.items():
        if k not in ("cards", "action_chips", "citations") and v is not None:
            merged[k] = v

    # 1. Merge cards: preserve existing, enrich matching product_id, append new
    a_cards: List[Dict[str, Any]] = list(a.get("cards") or [])
    b_cards: List[Dict[str, Any]] = list(b.get("cards") or [])

    merged_cards: List[Dict[str, Any]] = []
    pid_index: Dict[str, int] = {}

    for c in a_cards:
        pid = c.get("product_id")
        card_copy = dict(c)
        if pid:
            pid_index[pid] = len(merged_cards)
        merged_cards.append(card_copy)

    for c in b_cards:
        pid = c.get("product_id")
        if pid and pid in pid_index:
            idx = pid_index[pid]
            merged_cards[idx].update(c)
        else:
            if pid:
                pid_index[pid] = len(merged_cards)
            merged_cards.append(dict(c))

    merged["cards"] = merged_cards

    # 2. Merge action chips: deduplicate preserving order
    a_chips: List[str] = list(a.get("action_chips") or [])
    b_chips: List[str] = list(b.get("action_chips") or [])
    merged["action_chips"] = list(dict.fromkeys(a_chips + b_chips))

    # 3. Merge citations: deduplicate by product_id or url
    a_citations: List[Dict[str, Any]] = list(a.get("citations") or [])
    b_citations: List[Dict[str, Any]] = list(b.get("citations") or [])
    merged_citations: List[Dict[str, Any]] = []
    seen_cite_keys = set()
    for cite in a_citations + b_citations:
        key = cite.get("product_id") or cite.get("url") or str(cite)
        if key not in seen_cite_keys:
            seen_cite_keys.add(key)
            merged_citations.append(dict(cite))
    merged["citations"] = merged_citations

    return merged


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

    # Synthesis & UI Output with typed reducer
    final_response: Optional[str]
    ui_payload: Annotated[Optional[UIPayload], ui_payload_reducer]
    is_cache_hit: Optional[bool]
    cache_tier: Optional[str]

    # Execution control with parallel-safe reducers
    current_node: Annotated[Optional[str], last_str_reducer]
    error_message: Annotated[Optional[str], last_str_reducer]
    relaxation_level: Annotated[Optional[str], last_str_reducer]


__all__ = [
    "AgentState",
    "UIPayload",
    "UICard",
    "GroundingCitation",
    "ui_payload_reducer",
    "merge_dicts",
    "last_str_reducer",
]
