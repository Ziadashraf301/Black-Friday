"""
Entity Extractor Factory.
Instantiates the requested extraction strategy (Regex, Jev, or Hybrid).
"""
from ai.extractor.base import BaseEntityExtractor
from ai.extractor.regex_extractor import RegexEntityExtractor
from ai.extractor.jev_extractor import JevEntityExtractor
from ai.extractor.hybrid_extractor import HybridEntityExtractor


class EntityExtractorFactory:
    """Factory creating appropriate entity extractor strategy."""

    @staticmethod
    def get_extractor(strategy: str = "regex") -> BaseEntityExtractor:
        if strategy == "jev":
            return JevEntityExtractor()
        elif strategy == "hybrid":
            return HybridEntityExtractor()
        return RegexEntityExtractor()


# Default singleton instance
entity_extractor = RegexEntityExtractor()

__all__ = ["EntityExtractorFactory", "entity_extractor"]
