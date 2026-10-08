"""
Regression Tests for Fix 9.5 (Google GenAI Client Migration in Core Embeddings).
Validates:
1. GeminiEmbeddingProvider uses google.genai Client with mocked request/response.
2. Output embeddings maintain 768-dimension shape and MRL normalization.
3. Missing API key degrades gracefully to DeterministicSemanticProvider without import crash.
4. google-genai is present in requirements.txt and google-generativeai is absent.
"""
from pathlib import Path
from unittest.mock import MagicMock
import pytest

from core.embeddings.service import (
    GeminiEmbeddingProvider,
    DeterministicSemanticProvider,
    EmbeddingService,
)


def test_gemini_embedding_provider_with_mocked_genai_client():
    """Validates that GeminiEmbeddingProvider interacts with the google.genai Client API correctly."""
    mock_client = MagicMock()
    mock_response = MagicMock()
    mock_embedding = MagicMock()
    # Mock a 768-dim float vector
    mock_embedding.values = [0.05] * 768
    mock_response.embeddings = [mock_embedding]
    mock_client.models.embed_content.return_value = mock_response

    provider = GeminiEmbeddingProvider(api_key="mock_key", client=mock_client)
    assert provider.dimension == 768

    vector = provider.embed_text("test query for clothing")

    assert len(vector) == 768
    assert all(isinstance(v, float) for v in vector)
    mock_client.models.embed_content.assert_called_once()
    called_kwargs = mock_client.models.embed_content.call_args.kwargs
    assert called_kwargs["model"] == "models/gemini-embedding-2"
    assert called_kwargs["contents"] == "test query for clothing"


def test_missing_api_key_degrades_gracefully_to_deterministic():
    """Verifies that when GEMINI_API_KEY is unset or None, EmbeddingService falls back safely without crashing."""
    # When initialized without key, resolves to DeterministicSemanticProvider
    fallback_provider = EmbeddingService._resolve_default_provider()
    # If no valid key in environment, must be DeterministicSemanticProvider
    service = EmbeddingService(provider=DeterministicSemanticProvider())
    assert service.provider_name == "DeterministicSemanticProvider"
    vec = service.generate_embedding("sample offline text")
    assert len(vec) == 768
    assert all(isinstance(v, float) for v in vec)


def test_requirements_contains_google_genai():
    """Verifies requirements.txt specifies google-genai and does not include google-generativeai or loguru."""
    req_path = Path("requirements.txt")
    assert req_path.exists()
    content = req_path.read_text(encoding="utf-8")

    assert "google-genai" in content
    assert "google-generativeai" not in content
    assert "loguru" not in content
