"""
Terminal Refusal Node (Phase 3).
Halts graph execution immediately on adversarial attacks, returning deterministic refusal.
"""
from typing import Dict, Any
from langchain_core.messages import AIMessage
from ai.workflow.state import AgentState
from ai.guardrails.base import STANDARD_REFUSAL_MESSAGE
from core.logging import get_logger

logger = get_logger(__name__)


def refusal_node(state: AgentState) -> Dict[str, Any]:
    """
    Terminal node triggered when query is deemed adversarial (P_adv >= 0.80).
    Emits standard security refusal message and marks execution safe=False.
    """
    refusal_text = STANDARD_REFUSAL_MESSAGE
    logger.warning(
        f"[GRAPH: REFUSAL-NODE] Halting execution on adversarial query. "
        f"P_adv={state.get('adversarial_prob', 1.0):.4f}"
    )

    ai_msg = AIMessage(
        content=refusal_text,
        additional_kwargs={"security_refusal": True, "adversarial_probability": state.get("adversarial_prob", 1.0)}
    )

    return {
        "messages": [ai_msg],
        "final_response": refusal_text,
        "ui_payload": {
            "type": "security_refusal",
            "message": refusal_text,
        },
        "current_node": "refusal_node",
    }


__all__ = ["refusal_node"]
