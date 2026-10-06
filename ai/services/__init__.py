"""
AI Domain Services Layer.
Single entry point for all AI business and orchestration services:
- EmbeddingService (multimodal vector embeddings)
- ProductSearchService (hybrid vector & fulltext catalog search)
- GuardrailService (System-1 safety, routing & extraction orchestrator)
"""
from core.embeddings import (
    embedding_service,
    EmbeddingService,
    BaseEmbeddingProvider,
    GeminiEmbeddingProvider,
    DeterministicSemanticProvider,
)
from ai.services.search_service import search_service, ProductSearchService
from ai.services.guardrail_service import guardrail_service, GuardrailService

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
