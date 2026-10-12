"""
AI Domain Services Layer.
Single entry point for all AI business and orchestration services:
- EmbeddingService (multimodal vector embeddings)
- ProductSearchService (hybrid vector & fulltext catalog search)
- GuardrailService (System-1 safety, routing & extraction orchestrator)
"""

from ai.services.guardrail_service import GuardrailService, guardrail_service
from ai.services.search_service import ProductSearchService, search_service
from core.embeddings import (
    BaseEmbeddingProvider,
    DeterministicSemanticProvider,
    EmbeddingService,
    GeminiEmbeddingProvider,
    embedding_service,
)

__all__ = [
    "embedding_service",
    "EmbeddingService",
    "BaseEmbeddingProvider",
    "GeminiEmbeddingProvider",
    "DeterministicSemanticProvider",
    "search_service",
    "ProductSearchService",
    "guardrail_service",
    "GuardrailService",
]
