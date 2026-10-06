"""
Response Synthesis Node for LangGraph (Phase 3).
Delegates RAG augmentation, model inference, and UI payload assembly
to the dedicated SynthesisService.
"""
from typing import Dict, Any
from ai.workflow.state import AgentState
from ai.services.synthesis_service import synthesis_service


def response_synthesis_node(state: AgentState) -> Dict[str, Any]:
    """
    Final synthesis node before [END].
    Delegates to SynthesisService for RAG grounding and payload construction.
    """
    return synthesis_service.synthesize(state)
