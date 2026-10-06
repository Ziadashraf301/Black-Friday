"""
Automated Adversarial Strike Tracker & Ban Gateway Engine (Phase 4 - Task P4-07).
Tracks security policy violation strikes per user/IP in Redis (TTL 24 hours).
Enforces a 3-strike limit with an automatic 24-hour lockout.
"""
from typing import Optional, Dict
import time
from core.cache.redis_client import RedisCacheManager, cache_manager
from core.logging import get_logger

logger = get_logger(__name__)


class StrikeTracker:
    """
    Automated security strike counter and lockout enforcement.
    Prevents repeated prompt injection, jailbreaks, and adversarial attacks.
    """

    STRIKE_PREFIX = "security:strikes"
    BAN_PREFIX = "security:banned"
    BAN_TTL_SECONDS = 86400  # 24 hours in seconds
    MAX_STRIKES = 3

    def __init__(self, cache: Optional[RedisCacheManager] = None):
        self._cache = cache or cache_manager
        self._in_memory_strikes: Dict[str, int] = {}
        self._in_memory_bans: Dict[str, float] = {}

    def is_banned(self, identifier: Optional[str]) -> bool:
        """
        Checks whether the specified user or IP is currently locked out.
        Returns True if banned, False otherwise. Sub-millisecond evaluation (<0.1ms).
        """
        if not identifier:
            return False

        clean_id = identifier.strip().lower()

        # 1. Check Redis
        if self._cache.is_available and self._cache.client is not None:
            try:
                ban_key = f"{self.BAN_PREFIX}:{clean_id}"
                val = self._cache.client.get(ban_key)
                if val:
                    return True
            except Exception as e:
                logger.debug(f"[SECURITY: STRIKES] Redis check error: {e}")

        # 2. Check in-memory store
        if clean_id in self._in_memory_bans:
            if time.time() < self._in_memory_bans[clean_id]:
                return True
            else:
                del self._in_memory_bans[clean_id]

        return False

    def record_strike(self, identifier: Optional[str], ban_ttl: Optional[int] = None) -> int:
        """
        Increments strike counter for a malicious or adversarial query.
        Returns the updated strike count. If count >= MAX_STRIKES (3), locks out user for 24h (or ban_ttl).
        """
        if not identifier:
            return 0

        clean_id = identifier.strip().lower()
        strike_key = f"{self.STRIKE_PREFIX}:{clean_id}"
        ban_key = f"{self.BAN_PREFIX}:{clean_id}"
        ttl = ban_ttl if ban_ttl is not None else self.BAN_TTL_SECONDS
        strikes = 1
        redis_success = False

        # 1. Update Redis
        if self._cache.is_available and self._cache.client is not None:
            try:
                raw_client = self._cache.client
                strikes = raw_client.incr(strike_key)
                if strikes == 1:
                    raw_client.expire(strike_key, ttl)
                if strikes >= self.MAX_STRIKES:
                    raw_client.setex(ban_key, ttl, "BANNED_FOR_24H")
                    logger.warning(
                        f"[SECURITY: GATEWAY] User/IP '{clean_id}' reached {strikes} strikes. "
                        f"AUTOMATIC LOCKOUT APPLIED (TTL={ttl}s)."
                    )
                redis_success = True
            except Exception as e:
                logger.debug(f"[SECURITY: STRIKES] Redis incr error: {e}")

        if not redis_success:
            strikes = self._in_memory_strikes.get(clean_id, 0) + 1
            self._in_memory_strikes[clean_id] = strikes

        # 2. In-memory backup tracking
        self._in_memory_strikes[clean_id] = strikes
        if strikes >= self.MAX_STRIKES:
            self._in_memory_bans[clean_id] = time.time() + ttl
            logger.warning(
                f"[SECURITY: GATEWAY] User/IP '{clean_id}' reached {strikes} strikes. "
                f"In-memory lockout applied."
            )

        return strikes

    def reset_strikes(self, identifier: Optional[str]) -> None:
        """Resets strikes and lifts ban for testing or administrative action."""
        if not identifier:
            return

        clean_id = identifier.strip().lower()
        if self._cache.is_available:
            try:
                self._cache.delete_key(f"{self.STRIKE_PREFIX}:{clean_id}")
                self._cache.delete_key(f"{self.BAN_PREFIX}:{clean_id}")
            except Exception:
                pass

        self._in_memory_strikes.pop(clean_id, None)
        self._in_memory_bans.pop(clean_id, None)
        logger.info(f"[SECURITY: GATEWAY] Strikes and lockout reset for '{clean_id}'")


# Global singleton instance
strike_tracker = StrikeTracker()

__all__ = ["strike_tracker", "StrikeTracker"]
