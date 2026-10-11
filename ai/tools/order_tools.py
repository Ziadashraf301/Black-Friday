"""
Order Tracking & Policy Retrieval Tool (Phase 3).
Provides order fulfillment status, return window policies, shipping estimates,
and price match guarantee information.
Connected to PostgreSQL (user_purchases, curated_products) and PolicyKnowledgeBase.
"""
from typing import Optional, Dict, Any, List
from datetime import datetime, timedelta, timezone
import re
import zlib
from sqlalchemy import text

from ai.schemas import OrderStatus
from ai.tools.policy_kb import policy_kb
from core.db.repository import BlackFridayRepository
from core.logging import get_logger

logger = get_logger(__name__)


class OrderSupportTool:
    """Handles order status inquiries and store policy lookups backed by DB and Policy KB."""

    def __init__(self):
        self._repo: Optional[BlackFridayRepository] = None

    @property
    def repo(self) -> Optional[BlackFridayRepository]:
        if self._repo is None:
            try:
                self._repo = BlackFridayRepository()
            except Exception as e:
                logger.warning(f"[TOOL: ORDER] PostgreSQL repository init failed: {e}")
                self._repo = None
        return self._repo

    # Fixed demo orders that exist for every shopper (offline demos and tests); real orders always win.
    DEMO_ORDER_STATUS = {
        "ORD-9842": "Delivered",
        "ORD-1102": "Out for Delivery",
        "ORD-5541": "Shipped",
    }

    def track_order(self, order_id: Optional[str], user_id: Optional[str] = None) -> OrderStatus:
        """
        Looks up delivery status for one of the caller's own orders.
        A missing order id means the caller's most recent order. An order that does not exist or belongs
        to someone else is reported as "Not Found" (the same answer for both, so ids cannot be probed).
        """
        clean_id = (order_id or "").strip().upper()
        logger.info(f"[TOOL: ORDER] Tracking order_id={clean_id or 'latest'} for user={user_id}")

        db_order = self._query_database_order(clean_id, user_id)
        if db_order:
            return db_order

        if clean_id in self.DEMO_ORDER_STATUS:
            return self._demo_order(clean_id)

        return OrderStatus(order_id=clean_id or "LATEST", status="Not Found", carrier="", items_count=0)

    def _demo_order(self, order_id: str) -> OrderStatus:
        now = datetime.now(timezone.utc)
        return OrderStatus(
            order_id=order_id,
            status=self.DEMO_ORDER_STATUS[order_id],
            carrier="FedEx Priority",
            tracking_number=f"FX{zlib.crc32(order_id.encode('utf-8')) % 10000000000:010d}",
            estimated_delivery=(now + timedelta(days=2)).strftime("%B %d, %Y"),
            items=[{"product_id": "P00025442", "name": "Artisan Paisley Silk Kimono Shirt", "size": "L"}],
            return_eligible_until=(now + timedelta(days=60)).strftime("%B %d, %Y"),
        )

    def _query_database_order(self, order_id: str, user_id: Optional[str] = None) -> Optional[OrderStatus]:
        """Reads one purchase owned by `user_id` (by order number, else the latest). Guests own no orders."""
        if not self.repo or not user_id or not str(user_id).isdigit():
            return None

        order_num = None
        if order_id:
            num_match = re.search(r"\d+", order_id)
            if not num_match:
                return None
            order_num = int(num_match.group(0))

        query = text(
            """
            SELECT up.id, up.product_id, up.predicted_usd, up.purchased_at, cp.name, cp.sizes
            FROM user_purchases up
            LEFT JOIN curated_products cp ON up.product_id = cp.product_id
            WHERE up.user_id = :uid AND (CAST(:oid AS INTEGER) IS NULL OR up.id = :oid)
            ORDER BY up.purchased_at DESC
            LIMIT 1;
            """
        )
        try:
            with self.repo.engine.connect() as conn:
                row = conn.execute(query, {"uid": int(user_id), "oid": order_num}).fetchone()
        except Exception as e:
            logger.warning(f"[TOOL: ORDER] DB order query failed: {e}")
            return None
        if not row:
            return None

        p_id, p_pid, p_usd, p_date, p_name, p_sizes = row
        p_dt = p_date if p_date.tzinfo else p_date.replace(tzinfo=timezone.utc)
        age_days = (datetime.now(timezone.utc) - p_dt).days
        status = "Delivered" if age_days >= 3 else "In Transit" if age_days >= 1 else "Processing"
        return OrderStatus(
            order_id=f"ORD-{p_id}",
            status=status,
            carrier="FedEx Priority",
            tracking_number=f"FX{p_id:010d}",
            estimated_delivery=(p_dt + timedelta(days=3)).strftime("%B %d, %Y"),
            items=[
                {
                    "product_id": p_pid,
                    "name": p_name or f"Catalog Item {p_pid}",
                    "size": p_sizes[0] if (p_sizes and isinstance(p_sizes, list)) else "M",
                    "price": float(p_usd) if p_usd else 0.0,
                }
            ],
            return_eligible_until=(p_dt + timedelta(days=60)).strftime("%B %d, %Y"),
        )

    def get_policy(self, topic: str) -> Dict[str, Any]:
        """Fetches policy details from the Policy Knowledge Base."""
        return policy_kb.get_policy(topic)


# Global singleton instance
order_tool = OrderSupportTool()
