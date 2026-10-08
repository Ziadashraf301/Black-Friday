import pytest
from apps.api.routes.bot import tokenize_for_stream


def test_tokenize_for_stream_exact_roundtrip():
    text = "Hello world! This is a test.\n\n- Bullet 1\n- Bullet 2\n   Indented text."
    tokens = tokenize_for_stream(text)
    reconstructed = "".join(tokens)
    assert reconstructed == text


def test_tokenize_for_stream_empty_and_spaces():
    assert tokenize_for_stream("") == []
    text = "   "
    assert "".join(tokenize_for_stream(text)) == text


def test_tokenize_for_stream_splits_into_chunks():
    text = "One two three four five"
    tokens = tokenize_for_stream(text)
    assert len(tokens) >= 5
    assert "".join(tokens) == text
