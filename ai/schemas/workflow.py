"""
LangGraph State & Tool Schema Definitions (Phase 3).
Contracts for product search, specs, bundles, cart management, and order support.
"""
from typing import List, Optional, Dict, Any
from pydantic import BaseModel, Field


class ProductSearchResult(BaseModel):
    """Unified schema for product search hits returned by search specialists."""
    product_id: str
    name: str
    category_name: str
    original_price: float
    discounted_price: float
    badge: Optional[str] = None
    sizes: List[str] = Field(default_factory=list)
    image_url: Optional[str] = None
    score: float = 1.0
    style: Optional[str] = None
    relaxation_level: Optional[str] = "TIER_1_STRICT"


class ProductDetails(BaseModel):
    """Deep product specifications, materials, and care instructions."""
    product_id: str
    name: str
    tagline: str
    description: str
    materials: List[str]
    care_instructions: str = "Dry clean or delicate cycle"
    sizes: List[str]
    original_price: float
    discounted_price: float
    badge: Optional[str] = None
    image_url: Optional[str] = None
    in_stock: bool = True
    gender: Optional[str] = None
    brand: Optional[str] = None


class BundleItem(BaseModel):
    """Recommended complementary product for market basket cross-selling."""
    product_id: str
    name: str
    category_name: str = "Apparel"
    price: float = 0.0
    image_url: Optional[str] = None
    lift: Optional[float] = Field(default=None, description="Apriori lift score")
    confidence: Optional[float] = None
    similarity: Optional[float] = None
    relationship_type: str = "apriori"
    savings_pct: float = 15.0
    bundle_tag: str = Field(default="Frequently Bought Together")


class CartItem(BaseModel):
    """Item present in user cart."""
    product_id: str
    name: str
    size: str = "M"
    quantity: int = 1
    price: float = 0.0
    unit_price: float = 0.0
    total_price: float = 0.0
    image_url: Optional[str] = None

    def model_post_init(self, __context: Any) -> None:
        if self.price == 0.0 and self.unit_price > 0.0:
            self.price = self.unit_price
        elif self.unit_price == 0.0 and self.price > 0.0:
            self.unit_price = self.price
        if self.total_price == 0.0:
            p = self.price or self.unit_price
            self.total_price = round(self.quantity * p, 2)


class CartState(BaseModel):
    """Aggregated cart state with discount computations."""
    user_id: str = "guest_user"
    session_id: str = "default_session"
    items: List[CartItem] = Field(default_factory=list)
    subtotal: float = 0.0
    discount: float = 0.0
    discount_total: float = 0.0
    total: float = 0.0
    final_total: float = 0.0
    item_count: int = 0

    def model_post_init(self, __context: Any) -> None:
        if self.total == 0.0 and self.final_total > 0.0:
            self.total = self.final_total
        elif self.final_total == 0.0 and self.total > 0.0:
            self.final_total = self.total
        if self.discount == 0.0 and self.discount_total > 0.0:
            self.discount = self.discount_total
        elif self.discount_total == 0.0 and self.discount > 0.0:
            self.discount_total = self.discount


class OrderStatus(BaseModel):
    """Order fulfillment status and tracking details."""
    order_id: str
    status: str
    carrier: str = "FedEx Priority"
    tracking_number: str = ""
    estimated_delivery: str = ""
    items_count: int = 1
    total_amount: float = 0.0
    items: List[Dict[str, Any]] = Field(default_factory=list)
    return_eligible_until: Optional[str] = None


class UserProfile(BaseModel):
    """Shopper memory profile cached in Redis/Postgres."""
    user_id: str
    name: Optional[str] = "Shopper"
    email: Optional[str] = None
    preferred_category: Optional[str] = None
    preferred_size: Optional[str] = None
    budget_affinity: Optional[str] = None
    cluster_persona: Optional[str] = None
    total_lifetime_spend: float = 0.0


class AgentResponsePayload(BaseModel):
    """Standardized structured response payload sent to the frontend."""
    message: str
    intent: str
    products: List[ProductSearchResult] = Field(default_factory=list)
    details: Optional[ProductDetails] = None
    bundles: List[BundleItem] = Field(default_factory=list)
    cart: Optional[CartState] = None
    order: Optional[OrderStatus] = None
    suggestions: List[str] = Field(default_factory=list)
    latency_ms: float = 0.0
