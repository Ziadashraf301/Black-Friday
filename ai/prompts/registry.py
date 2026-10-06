"""
LLMOps Prompt & Question Registry Engine.
Central versioned repository for System-1 prompts, questions, and choice criteria.
"""
from typing import Dict, Optional, List
from ai.schemas import PromptVersion


class PromptRegistry:
    """
    Central Registry for Versioned System-1 Prompts & Intent Criteria.
    Supports A/B testing, audit tracking, and runtime rollbacks.
    """

    _REGISTRY: Dict[str, PromptVersion] = {}
    _DEFAULT_VERSION: str = "v1.0.0"

    @classmethod
    def register(cls, prompt_version: PromptVersion) -> None:
        cls._REGISTRY[prompt_version.version] = prompt_version

    @classmethod
    def get(cls, version: Optional[str] = None) -> PromptVersion:
        target = version or cls._DEFAULT_VERSION
        if target not in cls._REGISTRY:
            raise KeyError(f"Prompt version '{target}' not found in PromptRegistry. Available: {list(cls._REGISTRY.keys())}")
        return cls._REGISTRY[target]

    @classmethod
    def list_versions(cls) -> List[str]:
        return list(cls._REGISTRY.keys())

    @classmethod
    def clear(cls) -> None:
        """Clears registered prompts (mainly for testing)."""
        cls._REGISTRY.clear()
