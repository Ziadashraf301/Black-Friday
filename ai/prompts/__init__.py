"""
Central Prompts Module for All AI Layers.
Contains versioned System-1 decision prompts, extractor questions, RAG synthesis,
domain steering responses, and guardrail refusal templates.
"""
from ai.schemas import QuestionSpec, PromptVersion
from ai.prompts.registry import PromptRegistry
from ai.prompts.router_prompts import V1_PROMPT, V2_MULTI_INTENT_PROMPT
from ai.prompts.extractor_prompts import V1_EXTRACTOR_PROMPT, V2_EXTRACTOR_PROMPT
from ai.prompts.synthesis_prompts import RAG_SYSTEM_INSTRUCTION, format_rag_prompt
from ai.prompts.steering_prompts import DOMAIN_STEERING_RESPONSES, GENERIC_STEERING_RESPONSE
from ai.prompts.guardrail_prompts import STANDARD_REFUSAL_MESSAGE

# Register canonical prompts on module initialization
PromptRegistry.register(V1_PROMPT)
PromptRegistry.register(V2_MULTI_INTENT_PROMPT)
PromptRegistry.register(V1_EXTRACTOR_PROMPT)
PromptRegistry.register(V2_EXTRACTOR_PROMPT)

__all__ = [
    "QuestionSpec",
    "PromptVersion",
    "PromptRegistry",
    "V1_PROMPT",
    "V2_MULTI_INTENT_PROMPT",
    "V1_EXTRACTOR_PROMPT",
    "V2_EXTRACTOR_PROMPT",
    "RAG_SYSTEM_INSTRUCTION",
    "format_rag_prompt",
    "DOMAIN_STEERING_RESPONSES",
    "GENERIC_STEERING_RESPONSE",
    "STANDARD_REFUSAL_MESSAGE",
]
