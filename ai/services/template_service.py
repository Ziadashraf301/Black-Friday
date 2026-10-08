"""
Deterministic Response Templating Service (Phase 4).
Formats structured Markdown assistant responses based on specialist node evidence.
Decouples template formatting from LLM synthesis orchestration (SRP).
"""
from typing import Dict, Any, List, Optional


class ResponseTemplateService:
    """Deterministic high-speed markdown template formatter for the shopping assistant."""

    @staticmethod
    def render(
        intent: str,
        query: str,
        retrieved: Optional[List[Dict[str, Any]]] = None,
        details: Optional[Dict[str, Any]] = None,
        bundles: Optional[List[Dict[str, Any]]] = None,
        cart: Optional[Dict[str, Any]] = None,
        order_status: Optional[Dict[str, Any]] = None,
        policy: Optional[Dict[str, Any]] = None,
    ) -> str:
        """Deterministic high-speed template formatting."""
        response_lines: List[str] = []
        retrieved = retrieved or []
        bundles = bundles or []

        if retrieved:
            rl = retrieved[0].get("relaxation_level", "TIER_1_STRICT")
            if rl == "TIER_2_RELAX_SIZE":
                response_lines.append(f"We expanded the size filter to show you top-rated Black Friday styles matching **'{query}'**:\n")
            elif rl == "TIER_3_RELAX_BUDGET":
                response_lines.append(f"We expanded the price range by up to 25% to find the best deals for **'{query}'**:\n")
            elif rl == "TIER_4_ZERO_DATA_DEALS":
                response_lines.append(f"We couldn't find exact matches for **'{query}'**, but check out our top-trending Black Friday doorbuster deals!\n")
            else:
                response_lines.append(f"Here are top-matched Black Friday selections for **'{query}'**:\n")

            for idx, p in enumerate(retrieved, start=1):
                badge = f" `[{p['badge']}]`" if p.get("badge") else ""
                response_lines.append(
                    f"**{idx}. [{p['product_id']}] {p['name']}**{badge}\n"
                    f"- **Price**: ~~${p.get('original_price', 0):.2f}~~ **${p.get('discounted_price', p.get('price', 0)):.2f}**\n"
                    f"- **Category**: {p.get('category_name')} | **Sizes**: {', '.join(p.get('sizes', []))}\n"
                )
        elif details:
            badge = f" `[{details.get('badge')}]`" if details.get("badge") else ""
            response_lines.append(f"### **[{details.get('product_id')}] {details.get('name')}**{badge}\n")
            if details.get("tagline"):
                response_lines.append(f"> *{details['tagline']}*\n")
            if details.get("description"):
                response_lines.append(f"{details['description']}\n")
            response_lines.append(f"- **Special Price**: ~~${details.get('original_price', 0):.2f}~~ **${details.get('discounted_price', details.get('price', 0)):.2f}**")
            response_lines.append(f"- **Materials**: {', '.join(details.get('materials', []))}")
            response_lines.append(f"- **Care**: {details.get('care_instructions')}")
            response_lines.append(f"- **Available Sizes**: {', '.join(details.get('sizes', []))}\n")
        elif bundles:
            response_lines.append("Here are curated styling pairings and Apriori bundle savings:\n")
            for b in bundles:
                rel = "Frequent Match" if b.get("relationship_type") == "apriori" else "Similar Style"
                response_lines.append(
                    f"- **[{b.get('product_id')}] {b.get('name')}** (${b.get('price', 0):.2f}) — *{rel}* (Save {b.get('savings_pct', 15.0):.0f}%)"
                )
        elif cart:
            item_count = cart.get("item_count", 0)
            items = cart.get("items", [])
            if item_count > 0:
                response_lines.append(f"Your shopping cart has been updated (**{item_count} item{'s' if item_count > 1 else ''}**):\n")
                for item in items:
                    response_lines.append(
                        f"- **{item.get('name')}** (Size: {item.get('size')}, Qty: {item.get('quantity')}) — **${item.get('total_price', 0):.2f}**"
                    )
                response_lines.append(f"\n**Subtotal**: ${cart.get('subtotal', 0.0):.2f}")
                if cart.get("discount_total", 0.0) > 0:
                    response_lines.append(f"**Holiday Promo (10% off $150+)**: -${cart['discount_total']:.2f}")
                response_lines.append(f"**Total**: **${cart.get('final_total', 0.0):.2f}**\n")
            else:
                response_lines.append("Your shopping cart is currently empty.")
        elif order_status:
            response_lines.append(f"### Order Status: **{order_status.get('order_id')}**\n")
            response_lines.append(f"- **Status**: **{order_status.get('status')}**")
            response_lines.append(f"- **Carrier**: {order_status.get('carrier')} (`{order_status.get('tracking_number')}`)")
            response_lines.append(f"- **Estimated Delivery**: {order_status.get('estimated_delivery')}")
            if order_status.get("return_eligible_until"):
                response_lines.append(f"- **Free Return Eligible Until**: {order_status['return_eligible_until']}")
        elif policy:
            response_lines.append(f"### {policy.get('title', 'Store Policy')}\n")
            response_lines.append(f"{policy.get('summary', '')}\n")
            if policy.get("conditions"):
                response_lines.append("**Key Guidelines**:")
                for cond in policy["conditions"]:
                    response_lines.append(f"- {cond}")
        else:
            response_lines.append(
                "I'm here to assist with your Black Friday shopping! "
                "You can search our catalog, ask about item details, get outfit bundles, or check active discounts."
            )

        return "\n".join(response_lines)


# Singleton instance export
template_service = ResponseTemplateService()

__all__ = ["ResponseTemplateService", "template_service"]
