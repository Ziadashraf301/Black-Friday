"""
Store Policy Knowledge Base (Phase 3).
Loads, indexes, and queries verified Black Friday customer policies from
data/store_policies.json for support and RAG augmentation.
"""
from typing import Dict, Any, List, Optional
import json
from pathlib import Path
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class PolicyKnowledgeBase:
    """Indexed store policy knowledge base loaded from structured storage."""

    def __init__(self, policy_file_path: Optional[Path] = None):
        self.file_path = policy_file_path or (settings.BASE_DIR / "data" / "store_policies.json")
        self._policies: Dict[str, Dict[str, Any]] = {}
        self._keyword_index: Dict[str, List[str]] = {}  # keyword -> list of topics
        self._load()

    def _load(self) -> None:
        if self.file_path.exists():
            try:
                with open(self.file_path, "r", encoding="utf-8") as f:
                    data = json.load(f)
                    self._policies = data.get("policies", {})
                    logger.info(f"[POLICY-KB] Loaded {len(self._policies)} store policy articles from {self.file_path}")
            except Exception as e:
                logger.error(f"[POLICY-KB] Failed to load policy file {self.file_path}: {e}")
                self._load_fallback_policies()
        else:
            logger.warning(f"[POLICY-KB] Policy file {self.file_path} not found. Using fallback policies.")
            self._load_fallback_policies()

        # Build reverse keyword index
        self._keyword_index.clear()
        for topic, pdata in self._policies.items():
            keywords = pdata.get("keywords", [])
            for kw in keywords:
                kw_lower = kw.lower().strip()
                if kw_lower not in self._keyword_index:
                    self._keyword_index[kw_lower] = []
                if topic not in self._keyword_index[kw_lower]:
                    self._keyword_index[kw_lower].append(topic)

    def _load_fallback_policies(self) -> None:
        self._policies = {
            "returns": {
                "topic": "returns",
                "title": "Extended Holiday Return Policy",
                "summary": "All items purchased during Black Friday are eligible for free returns until January 31, 2027.",
                "conditions": [
                    "Items must be unwashed, unworn, with original tags.",
                    "Complimentary return shipping labels via our portal.",
                ],
                "keywords": ["return", "returns", "refund", "exchange"],
            },
            "shipping": {
                "topic": "shipping",
                "title": "Black Friday Shipping Timelines",
                "summary": "Standard express delivery is 2-4 business days. Free shipping on orders over $75.",
                "conditions": [
                    "Carriers include FedEx Priority and UPS Ground.",
                    "Order by December 18 for guaranteed Christmas arrival.",
                ],
                "keywords": ["shipping", "ship", "delivery", "fedex", "ups", "arrive"],
            },
            "price_match": {
                "topic": "price_match",
                "title": "Black Friday Price Match Guarantee",
                "summary": "We refund price drops on purchases between Nov 1 and Dec 5 if price drops before Dec 25.",
                "conditions": ["Applies to identical in-stock items."],
                "keywords": ["price match", "price drop", "refund difference"],
            },
            "discounts": {
                "topic": "discounts",
                "title": "Promotional Badges & Tiered Savings",
                "summary": "Save up to 60% off clearance plus 10% instant discount on orders over $150.",
                "conditions": ["Discounts stack automatically at checkout."],
                "keywords": ["discount", "coupon", "promo", "sale"],
            },
        }

    def get_policy(self, topic: str) -> Dict[str, Any]:
        """
        Retrieves policy document by exact topic name, partial alias, or keyword match.
        """
        key = topic.lower().strip()

        # 1. Exact match
        if key in self._policies:
            return self._policies[key]

        # 2. Key contains or is contained
        for p_topic, p_val in self._policies.items():
            if p_topic in key or key in p_topic:
                return p_val

        # 3. Keyword index lookup
        for kw, topics in self._keyword_index.items():
            if kw in key or key in kw:
                if topics and topics[0] in self._policies:
                    return self._policies[topics[0]]

        # 4. Fallback general policy summary
        return {
            "title": "General Customer Care & Store Guidelines",
            "summary": "We offer 24/7 shopping concierge support, free holiday returns until Jan 31, and price match guarantees.",
            "topics_available": list(self._policies.keys()),
        }

    def search_policies(self, query: str) -> List[Dict[str, Any]]:
        """Searches across policy titles, summaries, and conditions."""
        q_lower = query.lower()
        matched: List[Dict[str, Any]] = []

        for topic, pdata in self._policies.items():
            score = 0
            if topic in q_lower:
                score += 3
            if pdata.get("title", "").lower() in q_lower or any(w in pdata.get("title", "").lower() for w in q_lower.split()):
                score += 2
            if any(kw in q_lower for kw in pdata.get("keywords", [])):
                score += 2

            if score > 0:
                matched.append(pdata)

        return matched if matched else [self.get_policy("general")]

    @property
    def all_policies(self) -> Dict[str, Dict[str, Any]]:
        return dict(self._policies)


# Singleton knowledge base instance
policy_kb = PolicyKnowledgeBase()
