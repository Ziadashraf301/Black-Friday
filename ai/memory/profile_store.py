"""
Long-Term User Profile & Personalization Store (Phase 3).
Stores shopper sizing preferences, favorite styles, and past categories
with GDPR right-to-be-forgotten compliance.
"""
from typing import Optional, Dict, Any, List
import json
from ai.schemas import UserProfile
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class UserProfileStore:
    """Manages long-term shopper preferences across sessions."""

    def __init__(self):
        self._cache: Dict[str, UserProfile] = {}

    def get_profile(self, user_id: str) -> UserProfile:
        """Retrieves user profile by ID or creates blank guest profile."""
        if user_id in self._cache:
            return self._cache[user_id]

        profile = UserProfile(
            user_id=user_id,
            preferred_sizes=[],
            favorite_styles=[],
            budget_tier=None,
            past_viewed_categories=[],
        )
        self._cache[user_id] = profile
        return profile

    def update_profile(
        self,
        user_id: str,
        size: Optional[str] = None,
        style: Optional[str] = None,
        category: Optional[str] = None,
        budget: Optional[float] = None,
    ) -> UserProfile:
        """Incrementally learns user preferences from conversation turns."""
        profile = self.get_profile(user_id)

        if size and size.upper() not in profile.preferred_sizes:
            profile.preferred_sizes.append(size.upper())

        if style and style not in profile.favorite_styles:
            profile.favorite_styles.append(style)

        if category and category not in profile.past_viewed_categories:
            profile.past_viewed_categories.append(category)

        if budget is not None:
            if budget <= 60:
                profile.budget_tier = "Budget Friendly (<$60)"
            elif budget <= 120:
                profile.budget_tier = "Mid Tier ($60-$120)"
            else:
                profile.budget_tier = "Premium / Investment (> $120)"

        self._cache[user_id] = profile
        return profile

    def delete_profile(self, user_id: str) -> bool:
        """GDPR Article 17 Right to Be Forgotten deletion."""
        logger.info(f"[MEMORY: PROFILE] Wiping user profile for user_id={user_id}")
        if user_id in self._cache:
            del self._cache[user_id]
            return True
        return False


# Global singleton instance
user_profile_store = UserProfileStore()
