"""
Base Guardrail Interfaces and Standard Security Messages.
"""
from abc import ABC, abstractmethod
from ai.schemas import SafetyEvaluationResult
from ai.prompts.guardrail_prompts import STANDARD_REFUSAL_MESSAGE

__all__ = ["STANDARD_REFUSAL_MESSAGE", "BaseSafetyFilter"]


class BaseSafetyFilter(ABC):
    """Abstract Strategy interface for safety filters."""

    @abstractmethod
    def evaluate(self, text: str) -> SafetyEvaluationResult:
        """Evaluates input query and returns a typed SafetyEvaluationResult."""
        pass
