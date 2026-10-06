"""
Tier-1 Semantic Entity Extractor using Jev (TypeSafe AI) System-1.
Classifies queries across all 17 catalog categories using calibrated softmax probabilities.
"""
from typing import List, Optional
from ai.schemas import ExtractedEntities
from ai.extractor.base import BaseEntityExtractor
from ai.extractor.regex_extractor import RegexEntityExtractor
from ai.extractor.taxonomy import CATEGORY_HIERARCHY
from ai.prompts.registry import PromptRegistry
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class JevEntityExtractor(BaseEntityExtractor):
    """
    Tier-1 Semantic Entity Extractor using Jev (TypeSafe AI) System-1.
    Classifies queries across all 17 catalog categories using calibrated softmax probabilities.
    """

    def __init__(self, api_key: Optional[str] = None, prompt_version: str = "v2.0.0-extractor"):
        self._prompt_version = prompt_version
        self._api_key = api_key or getattr(settings, "TYPESAFE_API_KEY", None)
        self._client = None
        if self._api_key:
            try:
                from typesafe_sdk import TypeSafeClient
                self._client = TypeSafeClient(api_key=self._api_key)
            except Exception as e:
                logger.warning(f"[ENTITY-EXTRACTOR: JEV] Could not initialize TypeSafeClient: {e}")

    def extract(self, query: str) -> ExtractedEntities:
        if not self._client:
            return RegexEntityExtractor().extract(query)

        try:
            from typesafe_sdk import Choice
            prompt_spec = PromptRegistry.get(self._prompt_version)
            questions = {
                qid: Choice(instructions=q.instructions, criteria=q.criteria)
                for qid, q in prompt_spec.questions.items()
                if q.question_type == "choice"
            }
            res = self._client.system_one(
                state=query,
                questions=questions,
            )

            categories: List[str] = []
            ans = res.choices.get("target_category") or res.choices.get("target_department")
            if ans and ans.choice != "None":
                top_cat = ans.choice
                categories.append(top_cat)

                # Scan softmax probabilities for high-probability alternative categories (>= 0.05, max 3)
                probs = ans.probabilities or {}
                sorted_probs = sorted(
                    [(k, v) for k, v in probs.items() if k != "None"],
                    key=lambda x: x[1],
                    reverse=True
                )
                for cat, p in sorted_probs[:3]:
                    if p >= 0.05 and cat not in categories:
                        categories.append(cat)

                # Hierarchical expansion: add parent departments
                expanded = list(categories)
                for cat in categories:
                    for parent in CATEGORY_HIERARCHY.get(cat, []):
                        if parent not in expanded:
                            expanded.append(parent)
                categories = expanded

            return ExtractedEntities(
                max_price=None,
                min_price=None,
                sizes=[],
                product_ids=[],
                categories=categories,
                materials=[],
                styles=[],
                extraction_strategy="jev_v2",
            )
        except Exception as e:
            logger.error(f"[ENTITY-EXTRACTOR: JEV] Jev extraction failed: {e}. Falling back to empty.")
            return ExtractedEntities(extraction_strategy="jev_error")


__all__ = ["JevEntityExtractor"]
