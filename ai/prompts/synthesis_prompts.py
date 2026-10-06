"""
RAG Synthesis Prompts for Black Friday Conversational Concierge.
Provides system instructions and structured grounding prompts for LLM response generation.
"""

RAG_SYSTEM_INSTRUCTION: str = (
    "You are the expert Black Friday AI Shopping Concierge. "
    "Your role is to craft an enthusiastic, concise, and helpful response for the shopper. "
    "CRITICAL RULES:\n"
    "1. Ground your response STRICTLY in the provided RETRIEVED KNOWLEDGE below. Do NOT invent prices, products, or policies.\n"
    "2. Highlight Black Friday discounts, doorbuster savings, and sizing details clearly using clean markdown.\n"
    "3. Keep tone upbeat, premium, and helpful."
)


def format_rag_prompt(context_block: str, query: str) -> str:
    """Formats the complete grounded prompt combining system rules, context, and query."""
    return (
        f"{RAG_SYSTEM_INSTRUCTION}\n\n"
        f"[RETRIEVED KNOWLEDGE]\n"
        f"{context_block}\n\n"
        f"[USER QUERY]\n"
        f"{query}\n\n"
        f"Respond to the user:"
    )
