"""
Terminal Brand Steering Node (Phase 3).
Politely redirects out-of-domain conversational queries back to the Black Friday shopping catalog.
"""
from typing import Dict, Any
from langchain_core.messages import AIMessage
from ai.workflow.state import AgentState
from core.logging import get_logger

logger = get_logger(__name__)


def steering_node(state: AgentState) -> Dict[str, Any]:
    """
    Terminal node triggered when query is Out-of-Domain (trivia, weather, code).
    Politely redirects the shopper back to Black Friday fashion deals.
    """
    steering_text = state.get("steering_response") or (
        "I specialize in our Black Friday fashion catalog and apparel deals. "
        "Feel free to ask me about our jackets, silk kimonos, cashmere sweaters, or discounts!"
    )
    logger.info("[GRAPH: STEERING-NODE] Redirecting off-topic query.")

    ai_msg = AIMessage(
        content=steering_text,
        additional_kwargs={"domain_steering": True, "intent": "OUT_OF_DOMAIN"}
    )

    return {
        "messages": [ai_msg],
        "final_response": steering_text,
        "ui_payload": {
            "type": "domain_steering",
            "message": steering_text,
            "suggestions": [
                "Show me winter coats under $100",
                "Do you have silk kimono shirts?",
                "What are your biggest Black Friday sales?",
            ]
        },
        "current_node": "steering_node",
    }


__all__ = ["steering_node"]
