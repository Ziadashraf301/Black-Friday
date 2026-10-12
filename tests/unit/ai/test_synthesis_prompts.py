"""AI-10: the synthesis prompt tells the model that retrieved text and the user query are data, not instructions."""

from ai.prompts.synthesis_prompts import RAG_SYSTEM_INSTRUCTION, format_rag_prompt


def test_system_instruction_marks_inputs_untrusted():
    assert "untrusted data" in RAG_SYSTEM_INSTRUCTION
    assert "[RETRIEVED KNOWLEDGE]" in RAG_SYSTEM_INSTRUCTION and "[USER QUERY]" in RAG_SYSTEM_INSTRUCTION


def test_prompt_keeps_rules_before_the_untrusted_sections():
    prompt = format_rag_prompt(context_block="CTX", query="Q")
    assert prompt.index("untrusted data") < prompt.index("CTX") < prompt.index("Q\n\nRespond")
