"""
RAG Synthesis Service for Black Friday Assistant.
Manages context augmentation, LLM model invocation from configuration,
and deterministic fallback generation.
"""
from typing import Dict, Any, List, Optional, TYPE_CHECKING
from langchain_core.messages import AIMessage

if TYPE_CHECKING:
    from ai.workflow.state import AgentState, UIPayload

from ai.prompts.synthesis_prompts import format_rag_prompt
from ai.services.template_service import ResponseTemplateService, template_service
from core.config import settings
from core.logging import get_logger

logger = get_logger(__name__)


class SynthesisService:
    """Service handling RAG augmentation, LLM inference, and rich UI payload assembly."""

    def __init__(
        self,
        model_name: Optional[str] = None,
        template_service_instance: Optional[ResponseTemplateService] = None,
        llm_client: Optional[Any] = None,
    ):
        self._configured_model = model_name or getattr(settings, "LLM_MODEL", "gemini-2.0-flash")
        self._templates = template_service_instance or template_service
        self._llm_client = llm_client

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
        client = self._llm_client
        if client is None:
            api_key = getattr(settings, "GEMINI_API_KEY", None)
            if not api_key or api_key == "test_api_key_placeholder":
                return None
            try:
                from google import genai
                client = genai.Client(api_key=api_key)
            except Exception as e:
                logger.debug(f"[SYNTHESIS-SERVICE] Could not initialize genai client: {e}")
                return None

        try:
            prompt = format_rag_prompt(context_block=context_block, query=query)
            response = client.models.generate_content(
                model=self._configured_model,
                contents=prompt,
            )
            if response and getattr(response, "text", None):
                return response.text.strip()
        except Exception as e:
            logger.debug(f"[SYNTHESIS-SERVICE] LLM generation with {self._configured_model} skipped: {e}")

        return None

    def deterministic_template(
        self,
        intent: str,
        query: str,
        retrieved: Optional[List[Dict[str, Any]]] = None,
        details: Optional[Dict[str, Any]] = None,
        bundles: Optional[List[Dict[str, Any]]] = None,
        cart: Optional[Dict[str, Any]] = None,
        order_status: Optional[Dict[str, Any]] = None,
        policy: Optional[Dict[str, Any]] = None,
    ) -> str:
        """Deterministic high-speed template formatting delegated to ResponseTemplateService."""
        return self._templates.render(
            intent=intent,
            query=query,
            retrieved=retrieved,
            details=details,
            bundles=bundles,
            cart=cart,
            order_status=order_status,
            policy=policy,
        )

    def synthesize(self, state: "AgentState") -> Dict[str, Any]:
        """Main synthesis orchestrator generating response text and unified UI payload."""
        intent = state.get("intent", "PRODUCT_SEARCH")
        query = state.get("query", "")
        retrieved = state.get("retrieved_products") or []
        details = state.get("product_details")
        bundles = state.get("bundle_recommendations") or []
        cart = state.get("cart")
        order_status = state.get("order_status")
        policy = state.get("policy_details")

        # Determine UI Payload type and legacy data
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

        # Preserve and merge aggregated specialist UI elements
        existing_ui = state.get("ui_payload") or {}
        cards = list(existing_ui.get("cards") or [])

        # Fallback card assembly if no specialist cards were present in state
        if not cards:
            if retrieved:
                for p in retrieved:
                    pid = p.get("product_id")
                    if pid:
                        cards.append({
                            "type": "PRODUCT_CARD",
                            "product_id": pid,
                            "name": p.get("name"),
                            "price": p.get("discounted_price", p.get("price")),
                            "original_price": p.get("original_price"),
                            "image_url": p.get("image_url", f"/products/{pid}.jpg"),
                            "badge": p.get("badge", "Catalog Item"),
                        })
            elif details and details.get("product_id"):
                cards.append({
                    "type": "PRODUCT_DETAIL_CARD",
                    "product_id": details.get("product_id"),
                    "name": details.get("name"),
                    "price": details.get("discounted_price", details.get("price")),
                    "original_price": details.get("original_price"),
                    "image_url": details.get("image_url", f"/products/{details.get('product_id')}.jpg"),
                    "materials": details.get("materials", []),
                    "care_instructions": details.get("care_instructions"),
                    "sizes": details.get("sizes", []),
                    "stock_status": details.get("stock_status", "IN_STOCK"),
                })
            elif bundles:
                for b in bundles:
                    pid = b.get("product_id")
                    if pid:
                        cards.append({
                            "type": "BUNDLE_CARD",
                            "product_id": pid,
                            "name": b.get("name"),
                            "price": b.get("price"),
                            "bundle_price": b.get("bundle_price"),
                            "discount_pct": b.get("discount_pct", "15% OFF"),
                            "image_url": b.get("image_url", f"/products/{pid}.jpg"),
                        })

        default_chips = ["View Cart", "Checkout Now", "Top Deals"]
        existing_chips = list(existing_ui.get("action_chips") or [])
        action_chips = list(dict.fromkeys(existing_chips + default_chips))

        citations = list(existing_ui.get("citations") or [])
        if not citations and cards:
            for c in cards:
                pid = c.get("product_id")
                if pid:
                    citations.append({
                        "product_id": pid,
                        "title": c.get("name", f"Product {pid}"),
                        "url": f"/shopper/browse/{pid}",
                        "price": float(c.get("discounted_price") or c.get("bundle_price") or c.get("price") or 0.0),
                        "badge": c.get("badge") or c.get("discount_pct") or "Deal",
                    })

        final_ui_payload: Dict[str, Any] = {
            "type": ui_type,
            "data": ui_data,
            "cards": cards,
            "action_chips": action_chips,
            "citations": citations,
            "relaxation_level": state.get("relaxation_level", "STRICT"),
        }

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
                    ui_payload=final_ui_payload,
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
                    ui_payload=final_ui_payload,
                    intent=intent,
                )
            except Exception as e:
                logger.debug(f"[SYNTHESIS-SERVICE] Failed to store in cache: {e}")

        return {
            "messages": [ai_msg],
            "final_response": final_text,
            "ui_payload": final_ui_payload,
            "is_cache_hit": False,
            "cache_tier": "NONE",
            "current_node": "response_synthesis_node",
        }


# Global synthesis service instance
synthesis_service = SynthesisService()
