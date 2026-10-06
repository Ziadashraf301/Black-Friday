"""
Fast Intent Classifier & Conversational Steering Engine.
Provides direct canonical intent resolution without backward compatibility aliases,
domain steering for out-of-domain interactions, and rule-based offline classification.
"""
from typing import Tuple, List
import re

from ai.schemas import IntentType
from ai.prompts.steering_prompts import DOMAIN_STEERING_RESPONSES, GENERIC_STEERING_RESPONSE
from core.logging import get_logger

logger = get_logger(__name__)


class IntentClassifier:
    """Fast lexical and pattern-based classifier for canonical shopping intents."""

    RULE_PATTERNS = [
        # Adversarial / jailbreak patterns
        (r"(?i)\b(?:ignore\s+(?:all\s+)?previous|system\s+prompt|drop\s+table|leak\s+keys|override\s+safety)\b", IntentType.ADVERSARIAL_BLOCKED),
        # Order Tracking, fulfillment & Store Policies (returns, shipping, exchanges)
        (r"(?i)\b(?:where\s+is\s+my\s+order|track\s+order|tracking\s+number|delivery\s+status|ord-\d+|return\s+policy|returns?|refunds?|exchange\s+policy|shipping\s+policy)\b", IntentType.ORDER_SUPPORT),
        # Cart actions & mutations
        (r"(?i)\b(?:add\s+to\s+cart|put\s+in\s+my\s+cart|remove\s+from\s+cart|clear\s+cart|shopping\s+cart|change\s+size|view\s+cart)\b", IntentType.CART_ACTIONS),
        # Deals & promotions
        (r"(?i)\b(?:black\s+friday|cyber\s+monday|deal|discount|promo|coupon|clearance|sale|percentage\s+off)\b", IntentType.DEALS_PROMOTIONS),
        # Bundle & styling recommendations
        (r"(?i)\b(?:pair\s+well|recommend\s+a\s+complete|stylish\s+bundle|frequently\s+buy\s+together|capsule\s+collection|outfit\s+matching)\b", IntentType.BUNDLE_RECOMMENDATIONS),
        # Product details & specs
        (r"(?i)\b(?:made\s+of\s+100%|material\s+details|sizing\s+fit|what\s+kind\s+of\s+leather|full\s+specs|shrink\s+in\s+hot\s+water|fabric)\b", IntentType.PRODUCT_DETAILS),
        # Out-of-domain chitchat
        (r"(?i)\b(?:capital\s+city|write\s+a\s+python|weather\s+forecast|world\s+cup|romantic\s+poem)\b", IntentType.OUT_OF_DOMAIN),
        # Product search & discovery (default shopping)
        (r"(?i)\b(?:looking\s+for|show\s+me|find|do\s+you\s+have|i\s+need\s+a|search)\b", IntentType.PRODUCT_SEARCH),
    ]

    @classmethod
    def resolve_intent_type(cls, raw_intent: str) -> IntentType:
        """
        Resolves canonical IntentType directly from string value without alias mapping or fallbacks.
        Raises ValueError if intent is not a valid canonical IntentType.
        """
        clean = raw_intent.strip().upper()
        return IntentType(clean)

    @classmethod
    def classify_by_rules(cls, query: str) -> Tuple[IntentType, float]:
        """
        Fast lexical intent classification heuristic.
        Returns (IntentType, confidence).
        """
        for pattern, intent in cls.RULE_PATTERNS:
            if re.search(pattern, query):
                return intent, 0.90
        # If query mentions a specific product ID (e.g. P00025442)
        if re.search(r"\bP\d{5,8}\b", query):
            return IntentType.PRODUCT_DETAILS, 0.85
        return IntentType.PRODUCT_SEARCH, 0.70

    @classmethod
    def classify_multi_by_rules(cls, query: str) -> List[IntentType]:
        """
        Fast lexical multi-intent detection.
        Returns a list of all matching canonical intents (e.g. [PRODUCT_DETAILS, PRODUCT_SEARCH]).
        """
        matched_intents: List[IntentType] = []
        
        # Check all rule patterns for multiple triggers
        for pattern, intent in cls.RULE_PATTERNS:
            if re.search(pattern, query) and intent not in matched_intents:
                matched_intents.append(intent)

        # Check explicit product mention triggers
        has_product_id = bool(re.search(r"\bP\d{5,8}\b", query))
        if has_product_id and IntentType.PRODUCT_DETAILS not in matched_intents:
            matched_intents.append(IntentType.PRODUCT_DETAILS)

        # Check search constraints triggers (budget, size, category keywords)
        has_budget = bool(re.search(r"(?:under|less than|budget|below|\$)\s*\d+", query, re.IGNORECASE))
        has_category_terms = bool(re.search(r"\b(?:jacket|hoodie|boots|sneakers|shirt|denim|shoes|pants|jeans|sweater)\b", query, re.IGNORECASE))
        if (has_budget or has_category_terms) and IntentType.PRODUCT_SEARCH not in matched_intents:
            matched_intents.append(IntentType.PRODUCT_SEARCH)

        # Pairing / bundle triggers
        has_pairing = bool(re.search(r"\b(?:match(?:es)?|pair(?:s)?|outfit|go with|goes with)\b", query, re.IGNORECASE))
        if has_pairing and IntentType.BUNDLE_RECOMMENDATIONS not in matched_intents:
            matched_intents.append(IntentType.BUNDLE_RECOMMENDATIONS)

        if not matched_intents:
            matched_intents.append(IntentType.PRODUCT_SEARCH)

        return matched_intents

    @classmethod
    def get_steering_response(cls, query: str) -> str:
        """Returns tailored conversational steering for out-of-domain queries."""
        for pattern, resp in DOMAIN_STEERING_RESPONSES:
            if re.search(pattern, query):
                return resp
        return GENERIC_STEERING_RESPONSE
