"""
Base Entity Extractor Interface.
"""
from abc import ABC, abstractmethod
from ai.schemas import ExtractedEntities


class BaseEntityExtractor(ABC):
    """Abstract Strategy interface for entity extractors."""

    @abstractmethod
    def extract(self, query: str) -> ExtractedEntities:
        """Extracts structured entities from input query."""
        pass
