"""
RAG Synthesis Service for Black Friday Assistant.
Manages context augmentation, LLM model invocation from configuration,
and deterministic fallback generation.
"""
from typing import Dict, Any, List, Optional
from langchain_core.messages import AIMessage

from ai.workflow.state import AgentState
from ai.prompts.synthesis_prompts import format_rag_prompt
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class SynthesisService:
    """Service handling RAG augmentation, LLM inference, and rich UI payload assembly."""

    def __init__(self, model_name: Optional[str] = None):
        self._configured_model = model_name or getattr(settings, "LLM_MODEL", "gemini-2.0-flash")

    @property
    def model_name(self) -> str:
        return self._configured_model

    def build_rag_context(
        self,
        query: str,
        intent: str,
        retrieved: List[Dict[str, Any]],
        details: Optional[Dict[str, Any]],
        bundles: List[Dict[str, Any]],
        cart: Optional[Dict[str, Any]],
        order_status: Optional[Dict[str, Any]],
        policy: Optional[Dict[str, Any]],
    ) -> str:
        """Constructs a structured contextual grounding block."""
        context_parts: List[str] = [f"User Intent: {intent}", f"User Query: {query}"]

        if retrieved:
            context_parts.append("\n--- RETRIEVED PRODUCTS ---")
            for idx, p in enumerate(retrieved, start=1):
                context_parts.append(
                    f"{idx}. ID: {p.get('product_id')} | Name: {p.get('name')} | "
                    f"Original: ${p.get('original_price', 0):.2f} | Deal: ${p.get('discounted_price', 0):.2f} | "
                    f"Badge: {p.get('badge', 'N/A')} | Category: {p.get('category_name')} | Sizes: {p.get('sizes')}"
                )

        if details:
            context_parts.append("\n--- PRODUCT SPECIFICATIONS ---")
            context_parts.append(
                f"ID: {details.get('product_id')} | Name: {details.get('name')}\n"
                f"Tagline: {details.get('tagline')}\n"
                f"Description: {details.get('description')}\n"
                f"Price: ${details.get('discounted_price', 0):.2f} (Orig: ${details.get('original_price', 0):.2f})\n"
                f"Materials: {details.get('materials')}\n"
                f"Care: {details.get('care_instructions')}\n"
                f"Sizes: {details.get('sizes')}"
            )

        if bundles:
            context_parts.append("\n--- CURATED BUNDLE RECOMMENDATIONS ---")
            for b in bundles:
                context_parts.append(
                    f"- [{b.get('product_id')}] {b.get('name')} (${b.get('price', 0):.2f}) | "
                    f"Type: {b.get('relationship_type')} | Savings: {b.get('savings_pct', 0):.0f}%"
                )

        if cart:
            context_parts.append("\n--- CURRENT SHOPPING CART ---")
            context_parts.append(
                f"Items Count: {cart.get('item_count', 0)} | Subtotal: ${cart.get('subtotal', 0):.2f} | "
                f"Discounts: ${cart.get('discount_total', 0):.2f} | Total: ${cart.get('final_total', 0):.2f}"
            )
            for itm in cart.get("items", []):
                context_parts.append(
                    f"  * {itm.get('name')} (Size: {itm.get('size')}, Qty: {itm.get('quantity')}) = ${itm.get('total_price', 0):.2f}"
                )

        if order_status:
            context_parts.append("\n--- ORDER TRACKING STATUS ---")
            context_parts.append(
                f"Order ID: {order_status.get('order_id')} | Status: {order_status.get('status')} | "
                f"Carrier: {order_status.get('carrier')} ({order_status.get('tracking_number')}) | "
                f"Estimated Delivery: {order_status.get('estimated_delivery')} | "
                f"Return Eligible Until: {order_status.get('return_eligible_until')}"
            )

        if policy:
            context_parts.append("\n--- STORE POLICY KNOWLEDGE ---")
            context_parts.append(f"Title: {policy.get('title')}\nSummary: {policy.get('summary')}")
            if policy.get("conditions"):
                context_parts.append("Conditions:\n" + "\n".join(f"- {c}" for c in policy["conditions"]))

        return "\n".join(context_parts)

    def generate_llm_rag(self, context_block: str, query: str) -> Optional[str]:
        """Invokes configured LLM model with prompt grounding."""
        api_key = getattr(settings, "GEMINI_API_KEY", None)
        if not api_key or api_key == "test_api_key_placeholder":
            return None

        try:
            from google import genai
            client = genai.Client(api_key=api_key)
            prompt = format_rag_prompt(context_block=context_block, query=query)
            response = client.models.generate_content(
                model=self._configured_model,
                contents=prompt,
            )
            if response and response.text:
                return response.text.strip()
        except Exception as e:
            logger.debug(f"[SYNTHESIS-SERVICE] LLM generation with {self._configured_model} skipped: {e}")

        return None

    def deterministic_template(
        self,
        intent: str,
        query: str,
        retrieved: List[Dict[str, Any]],
        details: Optional[Dict[str, Any]],
        bundles: List[Dict[str, Any]],
        cart: Optional[Dict[str, Any]],
        order_status: Optional[Dict[str, Any]],
        policy: Optional[Dict[str, Any]],
    ) -> str:
        """Deterministic high-speed template formatting."""
        response_lines: List[str] = []

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
                    f"- **Price**: ~~${p['original_price']:.2f}~~ **${p['discounted_price']:.2f}**\n"
                    f"- **Category**: {p['category_name']} | **Sizes**: {', '.join(p['sizes'])}\n"
                )
        elif details:
            badge = f" `[{details['badge']}]`" if details.get("badge") else ""
            response_lines.append(f"### **[{details['product_id']}] {details['name']}**{badge}\n")
            response_lines.append(f"> *{details['tagline']}*\n")
            response_lines.append(f"{details['description']}\n")
            response_lines.append(f"- **Special Price**: ~~${details['original_price']:.2f}~~ **${details['discounted_price']:.2f}**")
            response_lines.append(f"- **Materials**: {', '.join(details['materials'])}")
            response_lines.append(f"- **Care**: {details['care_instructions']}")
            response_lines.append(f"- **Available Sizes**: {', '.join(details['sizes'])}\n")
        elif bundles:
            response_lines.append("Here are curated styling pairings and Apriori bundle savings:\n")
            for b in bundles:
                rel = "Frequent Match" if b.get("relationship_type") == "apriori" else "Similar Style"
                response_lines.append(
                    f"- **[{b['product_id']}] {b['name']}** (${b['price']:.2f}) — *{rel}* (Save {b.get('savings_pct', 15.0):.0f}%)"
                )
        elif cart:
            item_count = cart.get("item_count", 0)
            items = cart.get("items", [])
            if item_count > 0:
                response_lines.append(f"Your shopping cart has been updated (**{item_count} item{'s' if item_count > 1 else ''}**):\n")
                for item in items:
                    response_lines.append(
                        f"- **{item['name']}** (Size: {item['size']}, Qty: {item['quantity']}) — **${item['total_price']:.2f}**"
                    )
                response_lines.append(f"\n**Subtotal**: ${cart.get('subtotal', 0.0):.2f}")
                if cart.get("discount_total", 0.0) > 0:
                    response_lines.append(f"**Holiday Promo (10% off $150+)**: -${cart['discount_total']:.2f}")
                response_lines.append(f"**Total**: **${cart.get('final_total', 0.0):.2f}**\n")
            else:
                response_lines.append("Your shopping cart is currently empty.")
        elif order_status:
            response_lines.append(f"### Order Status: **{order_status['order_id']}**\n")
            response_lines.append(f"- **Status**: **{order_status['status']}**")
            response_lines.append(f"- **Carrier**: {order_status['carrier']} (`{order_status['tracking_number']}`)")
            response_lines.append(f"- **Estimated Delivery**: {order_status['estimated_delivery']}")
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

    def synthesize(self, state: AgentState) -> Dict[str, Any]:
        """Main synthesis orchestrator generating response text and UI payload."""
        intent = state.get("intent", "PRODUCT_SEARCH")
        query = state.get("query", "")
        retrieved = state.get("retrieved_products") or []
        details = state.get("product_details")
        bundles = state.get("bundle_recommendations") or []
        cart = state.get("cart")
        order_status = state.get("order_status")
        policy = state.get("policy_details")

        # Determine UI Payload
        ui_type = "general"
        ui_data: Dict[str, Any] = {}

        if retrieved:
            ui_type = "product_carousel"
            ui_data = {"products": retrieved}
        elif details:
            ui_type = "product_detail_modal"
            ui_data = {"product": details}
        elif bundles:
            ui_type = "bundle_card"
            ui_data = {"bundles": bundles}
        elif cart:
            ui_type = "cart_drawer"
            ui_data = {"cart": cart}
        elif order_status:
            ui_type = "order_tracking"
            ui_data = {"order": order_status}
        elif policy:
            ui_type = "policy_card"
            ui_data = {"policy": policy}

        # Build RAG Context & Generate Response
        context_block = self.build_rag_context(
            query=query,
            intent=intent,
            retrieved=retrieved,
            details=details,
            bundles=bundles,
            cart=cart,
            order_status=order_status,
            policy=policy,
        )

        llm_text = self.generate_llm_rag(context_block, query)
        final_text = (
            llm_text
            if llm_text
            else self.deterministic_template(
                intent=intent,
                query=query,
                retrieved=retrieved,
                details=details,
                bundles=bundles,
                cart=cart,
                order_status=order_status,
                policy=policy,
            )
        )

        ai_msg = AIMessage(
            content=final_text,
            additional_kwargs={
                "intent": intent,
                "ui_type": ui_type,
            },
        )

        # Cache synthesized response in Tier-0 (6h Redis) and Tier-1 (pgvector Semantic Cache)
        # Stateful mutations (CART_ACTIONS) must never be cached!
        if not state.get("is_cache_hit") and query and intent != "CART_ACTIONS":
            try:
                from ai.services.cache_service import cache_service
                cache_service.store_exact_llm_response(
                    raw_query=query,
                    response_message=final_text,
                    ui_payload={"type": ui_type, "data": ui_data},
                    target_intents=state.get("target_intents") or ([intent] if intent else []),
                    retrieved_products=retrieved,
                    product_details=details,
                    bundle_recommendations=bundles,
                    order_status=order_status,
                    policy_details=policy,
                )
                cache_service.store_vector_semantic_response(
                    query_text=query,
                    response_text=final_text,
                    ui_payload={"type": ui_type, "data": ui_data},
                    intent=intent,
                )
            except Exception as e:
                logger.debug(f"[SYNTHESIS-SERVICE] Failed to store in cache: {e}")


        return {
            "messages": [ai_msg],
            "final_response": final_text,
            "ui_payload": {
                "type": ui_type,
                "data": ui_data,
            },
            "is_cache_hit": False,
            "cache_tier": "NONE",
            "current_node": "response_synthesis_node",
        }


# Global synthesis service instance
synthesis_service = SynthesisService()
