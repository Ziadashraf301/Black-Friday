"""
Core embeddings package.
Provides multimodal and text embedding providers and service interfaces.
"""
from core.embeddings.service import (
    BaseEmbeddingProvider,
    GeminiEmbeddingProvider,
    DeterministicSemanticProvider,
    EmbeddingService,
    embedding_service,
)

__all__ = [
    "BaseEmbeddingProvider",
    "GeminiEmbeddingProvider",
    "DeterministicSemanticProvider",
    "EmbeddingService",
    "embedding_service",
]
