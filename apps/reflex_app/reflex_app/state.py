"""
Global state for 'THE Second Hand STORE' Reflex frontend.
Fully typed with rx.Base for 100% Reflex compiler compatibility.
Handles auth, catalog, filters, quick view, ONNX pricing, cart, history, and dashboard.
"""
import json
import httpx
from pathlib import Path
from typing import List, Dict, Any, Optional
from pydantic import BaseModel
import reflex as rx

import os

API_BASE_URL = os.getenv("API_BASE_URL", "http://127.0.0.1:8000")
INR_TO_USD_RATE: float = 80.0

_OCCUPATION_MAP = {
    0: "Student", 1: "Technology", 2: "Healthcare", 3: "Management",
    4: "Finance", 5: "Legal", 6: "Retail / Sales", 7: "Engineering",
    8: "Trades", 9: "Education", 10: "Government", 11: "Hospitality",
    12: "Agriculture", 13: "Media", 14: "Transport", 15: "Arts & Design",
    16: "Freelance", 17: "Real Estate", 18: "Retired", 19: "Homemaker", 20: "Other",
}


def _parse_auth_error(resp: httpx.Response) -> str:
    """Convert FastAPI/Pydantic validation errors to human-friendly messages."""
    try:
        body = resp.json()
    except Exception:
        return "Authentication service error. Please try again."

    detail = body.get("detail", "")
    if isinstance(detail, list):
        msgs = []
        for err in detail:
            if isinstance(err, dict):
                loc = " → ".join(str(l) for l in err.get("loc", [])[1:])
                msg = err.get("msg", "Invalid input").replace("String should have", "Must have")
                msgs.append(f"{loc.replace('_', ' ').title()}: {msg}" if loc else msg)
        return " | ".join(msgs) if msgs else "Please check your inputs and try again."
    if isinstance(detail, str):
        return detail
    return "Please check your inputs and try again."


def _format_cluster_persona(persona: str, cluster_id: int = 0) -> str:
    """Format technical persona strings into elegant retail member tiers (max 3 words)."""
    p = str(persona).strip()
    p_lower = p.lower()
    if "single female" in p_lower or "single woman" in p_lower:
        return "Urban Trendsetter"
    if "married male" in p_lower or "married men" in p_lower:
        return "Classic Connoisseur"
    if "single male" in p_lower or "single men" in p_lower:
        return "Modern Explorer"
    if "married female" in p_lower or "married women" in p_lower:
        return "Heritage Collector"
    if "high-value" in p_lower or "vip" in p_lower or "luxury" in p_lower:
        return "VIP Collector"
    if "budget" in p_lower or "casual" in p_lower:
        return "Casual Stylist"

    id_map = {
        0: "Urban Trendsetter",
        1: "Classic Connoisseur",
        2: "Modern Explorer",
        3: "Heritage Collector",
        4: "Prime Member",
        5: "Vintage Curator",
    }
    words = [w for w in p.split() if w not in ("<=", ">=", "<", ">", "=", "==")]
    if 1 <= len(words) <= 3 and not any(ch in p for ch in ("<", ">", "=", "_")):
        return " ".join(words).title()
    return id_map.get(cluster_id, "Preferred Member")


def _format_demographic_group(dim: str, raw_cat: Any) -> str:
    """Format technical category values into clean readable labels."""
    cat_str = str(raw_cat).strip()
    if dim == "marital_status":
        if cat_str in ("0", "Single"):
            return "Single"
        if cat_str in ("1", "Married"):
            return "Married"
        return "Single" if cat_str == "0" else "Married"
    if dim == "gender":
        if cat_str.upper() == "M":
            return "Male"
        if cat_str.upper() == "F":
            return "Female"
        return cat_str
    if dim == "city_category":
        return f"City Tier {cat_str.upper()}"
    return cat_str


class CartItem(BaseModel):
    key: str = ""
    product_id: str = ""
    name: str = ""
    image_url: str = ""
    price: float = 0.0
    personalized_price: float = 0.0
    has_personalized: bool = False
    size: str = "M"
    quantity: int = 1
    product_category_1: int = 1
    product_category_2: Optional[int] = None
    product_category_3: Optional[int] = None
    price_display: str = "$0.00"
    personalized_display: str = "$0.00"


class PurchaseEntry(BaseModel):
    id: int = 0
    product_id: str = ""
    predicted_usd: float = 0.0
    price_display: str = "$0.00"
    purchased_at_display: str = ""


class DemographicRow(BaseModel):
    group: str = ""
    total_orders: int = 0
    avg_purchase_display: str = "$0.00"
    pct_display: str = "0.0%"
    pct_width: str = "0%"


class RecProduct(BaseModel):
    product_id: str = ""
    name: str = ""
    image_url: str = ""
    price: float = 0.0
    price_display: str = "$0.00"
    badge_label: str = ""


class ShoppingState(rx.State):
    """Core reactive state for the entire store."""

    # --- Auth state ---
    auth_token: str = ""
    user_name: str = ""
    user_id: int = 0
    user_email: str = ""
    user_gender: str = ""
    user_age: str = ""
    user_city: str = ""
    user_occupation: int = 0
    user_cluster_id: int = 0
    user_cluster_persona: str = ""
    auth_error: str = ""
    auth_loading: bool = False
    show_auth: bool = False
    auth_tab: str = "login"

    # --- Login form ---
    login_email: str = ""
    login_password: str = ""

    # --- Signup form ---
    signup_name: str = ""
    signup_email: str = ""
    signup_password: str = ""
    signup_gender: str = "M"
    signup_age: str = "26-35"
    signup_city: str = "A"
    signup_marital: int = 0
    signup_occupation: int = 1

    # --- Catalog state ---
    products: List[Dict[str, Any]] = []
    hero_product: Dict[str, Any] = {}
    is_loading: bool = True

    # --- Filters ---
    search_query: str = ""
    selected_category: str = "All"
    selected_gender: str = "All"
    selected_brand: str = "All"
    selected_style: str = "All"
    selected_season: str = "All"
    open_accordion: str = "category"

    # --- Active product modal ---
    active_product: Dict[str, Any] = {}
    is_quick_view_open: bool = False
    hero_selected_size: str = "M"
    active_selected_size: str = "M"

    # --- ONNX pricing ---
    cached_price_estimates: Dict[str, float] = {}
    estimated_price_usd: float = 0.0
    price_prediction_model: str = ""
    is_predicting_price: bool = False

    # --- Cart ---
    cart_items: List[CartItem] = []
    is_cart_open: bool = False
    checkout_error: str = ""

    # --- Purchase history ---
    purchase_history: List[PurchaseEntry] = []
    is_history_loading: bool = False
    show_history: bool = False

    # --- Owner dashboard ---
    dashboard_summary: Dict[str, Any] = {}
    dashboard_demographics: List[DemographicRow] = []
    dashboard_cache: Dict[str, List[DemographicRow]] = {}
    is_dashboard_loading: bool = False
    show_dashboard: bool = False
    dashboard_dimension: str = "gender"

    # --- Bot Assistant Drawer (Phase 4 Conversational AI) ---
    is_bot_open: bool = False
    bot_messages: List[Dict[str, str]] = []
    bot_input_text: str = ""
    bot_loading: bool = False
    bot_auth_warning: str = ""
    bot_auth_checked: bool = False
    bot_action_chips: List[str] = ["Top Deals Today", "Sale Products", "Under $50", "Style Advisor"]
    bot_mode: str = "text"  # "text" or "voice"
    bot_is_listening: bool = False
    bot_is_locked_out: bool = False
    bot_lockout_message: str = ""
    bot_active_cards: List[Dict[str, Any]] = []

    # ── Auth computed vars ──────────────────────────────────────────────
    @rx.var
    def is_authenticated(self) -> bool:
        return bool(self.auth_token)

    @rx.var
    def welcome_name(self) -> str:
        return self.user_name or "Shopper"

    @rx.var
    def user_persona_label(self) -> str:
        return self.user_cluster_persona or "Preferred Member"

    @rx.var
    def user_occupation_label(self) -> str:
        return _OCCUPATION_MAP.get(self.user_occupation, "Professional")

    @rx.var
    def user_gender_label(self) -> str:
        return "Female" if self.user_gender == "F" else "Male"

    # ── Catalog computed vars ───────────────────────────────────────────
    @rx.var
    def filtered_products(self) -> List[Dict[str, Any]]:
        result = []
        hero_id = self.hero_product.get("product_id")
        for p in self.products:
            if hero_id and p.get("product_id") == hero_id:
                continue
            if self.search_query.strip():
                q = self.search_query.lower()
                if not any(q in str(p.get(f, "")).lower()
                           for f in ("name", "tagline", "description", "category_name", "brand", "style")):
                    continue
            if self.selected_category != "All" and p.get("category_name") != self.selected_category:
                continue
            if self.selected_gender != "All" and p.get("gender") != self.selected_gender:
                continue
            if self.selected_brand != "All" and p.get("brand") != self.selected_brand:
                continue
            if self.selected_style != "All" and p.get("style") != self.selected_style:
                continue
            if self.selected_season != "All" and p.get("season") != self.selected_season:
                continue
            result.append(p)
        return result

    @rx.var
    def filtered_count(self) -> int:
        return len(self.filtered_products)

    # ── Cart computed vars ──────────────────────────────────────────────
    @rx.var
    def cart_count(self) -> int:
        return sum(i.quantity for i in self.cart_items)

    @rx.var
    def cart_subtotal(self) -> float:
        return round(sum(i.price * i.quantity for i in self.cart_items), 2)

    @rx.var
    def cart_personalized_total(self) -> float:
        """Total cart price paying the personalized member price where available."""
        total = 0.0
        for i in self.cart_items:
            unit_price = i.personalized_price if (i.has_personalized and i.personalized_price > 0) else i.price
            total += unit_price * i.quantity
        return round(total, 2)

    @rx.var
    def cart_subtotal_display(self) -> str:
        return f"${self.cart_subtotal:.2f}"

    @rx.var
    def cart_personalized_total_display(self) -> str:
        return f"${self.cart_personalized_total:.2f}"

    @rx.var
    def cart_savings_display(self) -> str:
        savings = max(0.0, self.cart_subtotal - self.cart_personalized_total)
        return f"${savings:.2f}"

    # ── Active product computed vars ────────────────────────────────────
    @rx.var
    def active_product_id(self) -> str:
        return str(self.active_product.get("product_id", ""))

    @rx.var
    def active_product_name(self) -> str:
        return str(self.active_product.get("name", ""))

    @rx.var
    def active_product_image(self) -> str:
        return str(self.active_product.get("image_url", ""))

    @rx.var
    def active_product_tagline(self) -> str:
        return str(self.active_product.get("tagline", ""))

    @rx.var
    def active_product_desc(self) -> str:
        return str(self.active_product.get("description", ""))

    @rx.var
    def active_product_orders(self) -> int:
        return int(self.active_product.get("order_count", 0))

    @rx.var
    def active_orders_display(self) -> str:
        return f"{self.active_product_orders:,} orders"

    @rx.var
    def active_product_original_price(self) -> float:
        return float(self.active_product.get("original_price", 0.0))

    @rx.var
    def active_product_sale_price(self) -> float:
        return float(self.active_product.get("discounted_price", 0.0))

    @rx.var
    def active_sale_price_display(self) -> str:
        return f"${self.active_product_sale_price:.2f}"

    @rx.var
    def active_original_price_display(self) -> str:
        return f"Original: ${self.active_product_original_price:.2f}"

    @rx.var
    def active_estimated_price_display(self) -> str:
        return f"${self.estimated_price_usd:.2f}"

    @rx.var
    def active_product_badge(self) -> str:
        return str(self.active_product.get("badge", "") or "")

    @rx.var
    def active_product_sizes(self) -> List[str]:
        return self.active_product.get("sizes", ["S", "M", "L", "XL"]) if self.active_product else ["S", "M", "L", "XL"]

    def _extract_recs(self, key: str, default_name: str, default_price: float, badge: str) -> List[RecProduct]:
        raw = self.active_product.get(key, []) if self.active_product else []
        active_id = str(self.active_product.get("product_id", ""))
        seen = {active_id}
        res = []
        for item in raw:
            pid = str(item.get("product_id", ""))
            if not pid or pid in seen:
                continue
            seen.add(pid)
            p = float(item.get("price", default_price))
            res.append(RecProduct(
                product_id=pid,
                name=str(item.get("name", default_name)),
                image_url=str(item.get("image_url", f"/products/{pid}.jpg")),
                price=p,
                price_display=f"${p:.2f}",
                badge_label=badge,
            ))
            if len(res) >= 3:
                break
        return res

    @rx.var
    def active_apriori_bundles_list(self) -> List[RecProduct]:
        return self._extract_recs("apriori_bundles", "Curated Bundle", 69.0, "Bundle Rule")

    @rx.var
    def active_item2vec_similars_list(self) -> List[RecProduct]:
        return self._extract_recs("item2vec_similars", "Similar Piece", 59.0, "Similar Style")

    # ── Dashboard computed vars ─────────────────────────────────────────
    @rx.var
    def dashboard_orders_display(self) -> str:
        orders = int(self.dashboard_summary.get("total_orders", 0))
        return f"{orders:,}" if orders else "0"

    @rx.var
    def dashboard_revenue_display(self) -> str:
        # Convert INR to USD by dividing by INR_TO_USD_RATE
        rev_inr = float(self.dashboard_summary.get("total_revenue", 0.0))
        rev_usd = rev_inr / INR_TO_USD_RATE
        if rev_usd >= 1_000_000:
            return f"${rev_usd / 1_000_000:.1f}M"
        if rev_usd >= 1_000:
            return f"${rev_usd / 1_000:.1f}K"
        return f"${rev_usd:.2f}"

    @rx.var
    def dashboard_users_display(self) -> str:
        users = int(self.dashboard_summary.get("total_users", 0))
        return f"{users:,}" if users else "0"

    @rx.var
    def dashboard_products_display(self) -> str:
        prods = int(self.dashboard_summary.get("total_products", 0))
        return f"{prods:,}" if prods else "0"

    @rx.var
    def dashboard_aov_display(self) -> str:
        # Convert INR to USD by dividing by INR_TO_USD_RATE
        aov_inr = float(self.dashboard_summary.get("avg_order_value", 0.0))
        aov_usd = aov_inr / INR_TO_USD_RATE
        return f"${aov_usd:.2f}"

    @rx.var
    def total_purchase_count(self) -> int:
        return len(self.purchase_history)

    @rx.var
    def total_spent_usd(self) -> float:
        return round(sum(p.predicted_usd for p in self.purchase_history), 2)

    @rx.var
    def total_spent_display(self) -> str:
        return f"${self.total_spent_usd:.2f}"

    # ── Auth actions ────────────────────────────────────────────────────
    def open_auth(self, tab: str = "login"):
        self.show_auth = True
        self.auth_tab = tab
        self.auth_error = ""

    def close_auth(self):
        self.show_auth = False
        self.auth_error = ""

    def set_auth_tab(self, tab: str):
        self.auth_tab = tab
        self.auth_error = ""

    def set_login_email(self, v: str): self.login_email = v
    def set_login_password(self, v: str): self.login_password = v
    def set_signup_name(self, v: str): self.signup_name = v
    def set_signup_email(self, v: str): self.signup_email = v
    def set_signup_password(self, v: str): self.signup_password = v
    def set_signup_gender(self, v: str): self.signup_gender = v
    def set_signup_age(self, v: str): self.signup_age = v
    def set_signup_city(self, v: str): self.signup_city = v

    def do_login(self):
        if not self.login_email or not self.login_password:
            self.auth_error = "Please enter your email and password."
            return
        self.auth_loading = True
        self.auth_error = ""
        try:
            with httpx.Client(timeout=8.0) as client:
                resp = client.post(
                    f"{API_BASE_URL}/auth/login",
                    json={"email": self.login_email.strip().lower(), "password": self.login_password}
                )
                if resp.status_code == 200:
                    self._apply_auth(resp.json())
                    self.show_auth = False
                    self.login_password = ""
                else:
                    self.auth_error = _parse_auth_error(resp)
        except Exception:
            self.auth_error = "Cannot reach authentication server. Please check your connection."
        finally:
            self.auth_loading = False

    def do_signup(self):
        if not self.signup_name or not self.signup_email or not self.signup_password:
            self.auth_error = "Name, email, and password are required."
            return
        if len(self.signup_password) < 6:
            self.auth_error = "Password must be at least 6 characters."
            return
        self.auth_loading = True
        self.auth_error = ""
        payload = {
            "name": self.signup_name.strip(),
            "email": self.signup_email.strip().lower(),
            "password": self.signup_password,
            "gender": self.signup_gender,
            "age": self.signup_age,
            "city_category": self.signup_city,
            "marital_status": self.signup_marital,
            "occupation": self.signup_occupation,
            "stay_in_current_city_years": "2",
        }
        try:
            with httpx.Client(timeout=10.0) as client:
                resp = client.post(f"{API_BASE_URL}/auth/signup", json=payload)
                if resp.status_code in (200, 201):
                    self._apply_auth(resp.json())
                    self.show_auth = False
                    self.signup_password = ""
                else:
                    self.auth_error = _parse_auth_error(resp)
        except Exception:
            self.auth_error = "Cannot reach server. Please try again."
        finally:
            self.auth_loading = False

    def _apply_auth(self, data: Dict[str, Any]):
        self.auth_token = str(data.get("access_token", ""))
        self.user_id = int(data.get("user_id", 0))
        self.user_name = str(data.get("name", ""))
        self.user_cluster_id = int(data.get("cluster_id", 0))
        raw_persona = str(data.get("cluster_persona", "Preferred Member"))
        self.user_cluster_persona = _format_cluster_persona(raw_persona, self.user_cluster_id)
        # Fetch full profile to populate gender, age, occupation, etc.
        self._fetch_me()
        # Batch fetch personalized pricing for all catalog items into local frontend cache
        self.fetch_batch_ai_price_estimates()
        # Preserve offline cart items and recalculate member pricing for each
        if self.cart_items:
            self._reprice_cart()

    def _fetch_me(self):
        if not self.auth_token:
            return
        try:
            with httpx.Client(timeout=5.0) as client:
                resp = client.get(
                    f"{API_BASE_URL}/auth/me",
                    headers={"Authorization": f"Bearer {self.auth_token}"}
                )
                if resp.status_code == 200:
                    me = resp.json()
                    self.user_gender = str(me.get("gender", ""))
                    self.user_age = str(me.get("age", ""))
                    self.user_city = str(me.get("city_category", ""))
                    self.user_occupation = int(me.get("occupation", 0))
                    self.user_cluster_id = int(me.get("cluster_id", 0))
                    raw_persona = str(me.get("cluster_persona", "Preferred Member"))
                    self.user_cluster_persona = _format_cluster_persona(raw_persona, self.user_cluster_id)
                    self.user_email = str(me.get("email", ""))
        except Exception:
            pass

    def do_logout(self):
        self.auth_token = ""
        self.user_name = ""
        self.user_id = 0
        self.user_email = ""
        self.user_cluster_persona = ""
        # Revert personalized prices back to catalog prices in cart
        updated_cart = []
        for i in self.cart_items:
            updated_cart.append(CartItem(
                key=i.key,
                product_id=i.product_id,
                name=i.name,
                image_url=i.image_url,
                price=i.price,
                personalized_price=0.0,
                has_personalized=False,
                size=i.size,
                quantity=i.quantity,
                product_category_1=i.product_category_1,
                product_category_2=i.product_category_2,
                product_category_3=i.product_category_3,
                price_display=f"${i.price:.2f}",
                personalized_display=f"${i.price:.2f}",
            ))
        self.cart_items = updated_cart
        self.purchase_history = []
        self.is_cart_open = False
        self.show_history = False
        self.show_dashboard = False

    # ── Catalog actions ─────────────────────────────────────────────────
    def load_catalog(self):
        self.is_loading = True
        try:
            with httpx.Client(timeout=4.0) as client:
                resp = client.get(f"{API_BASE_URL}/shopper/curated-catalog")
                if resp.status_code == 200:
                    self._set_catalog(resp.json())
                    self.is_loading = False
                    return
        except Exception:
            pass
        catalog_path = Path(__file__).resolve().parents[3] / "data" / "curated_products.json"
        if catalog_path.exists():
            with open(catalog_path, "r", encoding="utf-8") as f:
                self._set_catalog(json.load(f))
        self.is_loading = False

    def _set_catalog(self, data: List[Dict[str, Any]]):
        self.products = data
        hero = next((p for p in data if p.get("is_hero")), None)
        self.hero_product = hero if hero else (data[0] if data else {})

    def set_search_query(self, q: str):
        self.search_query = q

    def set_filter_category(self, v: str): self.selected_category = v
    def set_filter_gender(self, v: str): self.selected_gender = v
    def set_filter_brand(self, v: str): self.selected_brand = v
    def set_filter_style(self, v: str): self.selected_style = v
    def set_filter_season(self, v: str): self.selected_season = v

    def reset_filters(self):
        self.selected_category = "All"
        self.selected_gender = "All"
        self.selected_brand = "All"
        self.selected_style = "All"
        self.selected_season = "All"
        self.search_query = ""

    def toggle_accordion(self, s: str):
        self.open_accordion = "" if self.open_accordion == s else s

    # ── Product navigation ───────────────────────────────────────────────
    def open_product_detail(self, product_id: str):
        """Open quick-view for any product, seamlessly switching content."""
        target = next((p for p in self.products if p.get("product_id") == product_id), None)
        if not target and self.hero_product.get("product_id") == product_id:
            target = self.hero_product
        if not target:
            target = self._fetch_product_from_api(product_id)
        if target:
            self.active_product = target
            self.active_selected_size = "M"
            self.is_quick_view_open = True
            base_price = float(target.get("discounted_price", 49.90))
            self.estimated_price_usd = base_price
            self.price_prediction_model = "Catalog Price"
            if self.is_authenticated:
                self.fetch_ai_price_estimate(target)

    def _fetch_product_from_api(self, product_id: str) -> Optional[Dict[str, Any]]:
        try:
            with httpx.Client(timeout=3.0) as client:
                resp = client.get(f"{API_BASE_URL}/shopper/browse/{product_id}")
                if resp.status_code == 200:
                    d = resp.json()
                    return {
                        "product_id": product_id,
                        "name": f"Product {product_id}",
                        "tagline": "Curated Archive Piece",
                        "description": f"Archival collection item with {d.get('order_count', 0):,} verified purchases.",
                        "category_name": "Archive",
                        "product_category_1": 1,
                        "image_url": f"/products/{product_id}.jpg",
                        "original_price": 149.0,
                        "discounted_price": 79.0,
                        "order_count": d.get("order_count", 0),
                        "sizes": ["S", "M", "L", "XL"],
                        "badge": "Vintage",
                        "gender": "Unisex",
                        "brand": "Archive",
                        "style": "Vintage",
                        "season": "All-Season",
                        "apriori_bundles": [
                            {"product_id": pid, "name": f"Archive {pid}", "image_url": f"/products/{pid}.jpg", "price": 89.0}
                            for pid in d.get("apriori_bundles", [])[:3]
                        ],
                        "item2vec_similars": [
                            {"product_id": pid, "name": f"Similar {pid}", "image_url": f"/products/{pid}.jpg", "price": 69.0}
                            for pid in d.get("item2vec_similar", [])[:3]
                        ],
                    }
        except Exception:
            pass
        return None

    def navigate_to_rec(self, product_id: str):
        """Switch quick-view directly to a recommendation product without modal flicker."""
        self.open_product_detail(product_id)

    def set_active_size(self, size: str):
        self.active_selected_size = size

    def close_quick_view(self):
        self.is_quick_view_open = False
        self.active_product = {}

    def set_is_quick_view_open(self, val: bool):
        self.is_quick_view_open = val
        if not val:
            self.active_product = {}

    def set_is_cart_open(self, val: bool):
        self.is_cart_open = val

    def set_show_dashboard(self, val: bool):
        self.show_dashboard = val
        if val and not self.dashboard_summary:
            self.load_dashboard()

    def set_show_auth(self, val: bool):
        self.show_auth = val
        if not val:
            self.auth_error = ""

    # ── ONNX pricing ────────────────────────────────────────────────────
    def fetch_batch_ai_price_estimates(self):
        """Batch fetches personalized pricing quotes for all catalog items once upon login or catalog load."""
        if not self.auth_token or not self.products:
            return
        items_payload = [
            {
                "product_id": p.get("product_id", ""),
                "product_category_1": p.get("product_category_1", 1),
                "product_category_2": p.get("product_category_2"),
                "product_category_3": p.get("product_category_3"),
            }
            for p in self.products if p.get("product_id")
        ]
        if not items_payload:
            return
        try:
            with httpx.Client(timeout=6.0) as client:
                resp = client.post(
                    f"{API_BASE_URL}/shopper/predict-price-batch",
                    json={"items": items_payload},
                    headers={"Authorization": f"Bearer {self.auth_token}"}
                )
                if resp.status_code == 200:
                    data = resp.json()
                    for quote in data.get("quotes", []):
                        pid = quote.get("product_id")
                        p_usd = quote.get("predicted_usd")
                        if pid and p_usd is not None:
                            self.cached_price_estimates[pid] = round(float(p_usd), 2)
        except Exception:
            pass

    def fetch_ai_price_estimate(self, product: Dict[str, Any]):
        if not self.auth_token:
            return

        pid = product.get("product_id", "")
        # Check local Reflex frontend cache first to eliminate unnecessary HTTP round-trips while scrolling/browsing
        if pid and pid in self.cached_price_estimates:
            self.estimated_price_usd = self.cached_price_estimates[pid]
            self.price_prediction_model = "Production Champion (Cached)"
            return

        self.is_predicting_price = True
        payload = {
            "product_id": pid,
            "product_category_1": product.get("product_category_1", 1),
            "product_category_2": product.get("product_category_2"),
            "product_category_3": product.get("product_category_3"),
        }
        try:
            with httpx.Client(timeout=4.0) as client:
                resp = client.post(
                    f"{API_BASE_URL}/shopper/predict-price",
                    json=payload,
                    headers={"Authorization": f"Bearer {self.auth_token}"}
                )
                if resp.status_code == 200:
                    d = resp.json()
                    est = round(float(d.get("predicted_usd", 0.0)), 2)
                    self.estimated_price_usd = est
                    self.price_prediction_model = str(d.get("model_used", "ONNX"))
                    if pid:
                        self.cached_price_estimates[pid] = est
                    self.is_predicting_price = False
                    return
        except Exception:
            pass
        self.estimated_price_usd = float(product.get("discounted_price", 0.0))
        self.price_prediction_model = "Catalog Price"
        self.is_predicting_price = False

    # ── Hero ─────────────────────────────────────────────────────────────
    def set_hero_size(self, size: str):
        self.hero_selected_size = size

    # ── Cart ─────────────────────────────────────────────────────────────
    def _add_with_size(self, product_id: str, size: str):
        target = next((p for p in self.products if p.get("product_id") == product_id), None)
        if not target and self.hero_product.get("product_id") == product_id:
            target = self.hero_product
        if not target and self.active_product.get("product_id") == product_id:
            target = self.active_product
        if not target:
            return

        key = f"{product_id}_{size}"
        base_price = float(target.get("discounted_price", target.get("original_price", 49.99)))

        # Determine personalized price if logged in
        pers_price = 0.0
        has_pers = False
        if self.is_authenticated:
            if product_id in self.cached_price_estimates and self.cached_price_estimates[product_id] > 0:
                pers_price = self.cached_price_estimates[product_id]
                has_pers = True
            elif self.active_product.get("product_id") == product_id and self.estimated_price_usd > 0:
                pers_price = self.estimated_price_usd
                has_pers = True

        # Check existing item
        new_items = []
        found = False
        for item in self.cart_items:
            if item.key == key:
                found = True
                new_items.append(CartItem(
                    key=item.key,
                    product_id=item.product_id,
                    name=item.name,
                    image_url=item.image_url,
                    price=item.price,
                    personalized_price=pers_price if has_pers else (item.personalized_price if item.has_personalized else 0.0),
                    has_personalized=has_pers or item.has_personalized,
                    size=item.size,
                    quantity=item.quantity + 1,
                    product_category_1=item.product_category_1,
                    product_category_2=item.product_category_2,
                    product_category_3=item.product_category_3,
                    price_display=f"${item.price:.2f}",
                    personalized_display=f"${(pers_price if has_pers else (item.personalized_price if item.has_personalized else item.price)):.2f}",
                ))
            else:
                new_items.append(item)

        if not found:
            new_items.append(CartItem(
                key=key,
                product_id=target["product_id"],
                name=str(target.get("name", product_id)),
                image_url=str(target.get("image_url", f"/products/{product_id}.jpg")),
                price=base_price,
                personalized_price=pers_price if has_pers else 0.0,
                has_personalized=has_pers,
                size=size,
                quantity=1,
                product_category_1=int(target.get("product_category_1", 1)),
                product_category_2=target.get("product_category_2"),
                product_category_3=target.get("product_category_3"),
                price_display=f"${base_price:.2f}",
                personalized_display=f"${pers_price:.2f}" if has_pers else f"${base_price:.2f}",
            ))

        self.cart_items = new_items

    def _reprice_cart(self):
        """After login, recalculate and apply personalized member prices for all cart items."""
        if not self.auth_token or not self.cart_items:
            return
        try:
            items_payload = [
                {
                    "product_id": i.product_id,
                    "product_category_1": i.product_category_1,
                    "product_category_2": i.product_category_2,
                    "product_category_3": i.product_category_3,
                }
                for i in self.cart_items
            ]
            with httpx.Client(timeout=5.0) as client:
                resp = client.post(
                    f"{API_BASE_URL}/shopper/predict-price-batch",
                    json={"items": items_payload},
                    headers={"Authorization": f"Bearer {self.auth_token}"}
                )
                if resp.status_code == 200:
                    quotes = resp.json().get("quotes", [])
                    pid_to_price = {q["product_id"]: float(q["predicted_usd"]) for q in quotes}
                    updated_items = []
                    for item in self.cart_items:
                        p_price = pid_to_price.get(item.product_id, item.price * 0.85)
                        updated_items.append(CartItem(
                            key=item.key,
                            product_id=item.product_id,
                            name=item.name,
                            image_url=item.image_url,
                            price=item.price,
                            personalized_price=p_price,
                            has_personalized=True,
                            size=item.size,
                            quantity=item.quantity,
                            product_category_1=item.product_category_1,
                            product_category_2=item.product_category_2,
                            product_category_3=item.product_category_3,
                            price_display=f"${item.price:.2f}",
                            personalized_display=f"${p_price:.2f}",
                        ))
                    self.cart_items = updated_items
        except Exception:
            pass

    def add_to_cart(self, product_id: str):
        self._add_with_size(product_id, "M")

    def add_product_with_size(self, product_id: str, size: str):
        self._add_with_size(product_id, size)

    def add_active_to_cart(self):
        if self.active_product:
            self._add_with_size(
                self.active_product.get("product_id", ""),
                self.active_selected_size
            )
            self.is_cart_open = True

    def add_hero_to_cart(self):
        if self.hero_product:
            self._add_with_size(self.hero_product.get("product_id", ""), self.hero_selected_size)
            self.is_cart_open = True

    def remove_cart_item(self, key: str):
        self.cart_items = [i for i in self.cart_items if i.key != key]

    def toggle_cart(self):
        self.is_cart_open = not self.is_cart_open
        if self.is_cart_open:
            self.checkout_error = ""

    def checkout(self):
        """Record checkout purchases at personalized price, update history, and clear bag."""
        if not self.cart_items:
            self.is_cart_open = False
            self.checkout_error = ""
            return
        if not self.auth_token:
            # Prompt user to log in so they receive member price and order history
            self.open_auth("login")
            return

        self.checkout_error = ""
        success = False
        try:
            with httpx.Client(timeout=10.0) as client:
                headers = {"Authorization": f"Bearer {self.auth_token}"}
                batch_payload = {
                    "items": [
                        {
                            "product_id": item.product_id,
                            "product_category_1": item.product_category_1,
                            "product_category_2": item.product_category_2,
                            "product_category_3": item.product_category_3,
                            "quantity": item.quantity,
                        }
                        for item in self.cart_items
                    ]
                }
                resp = client.post(
                    f"{API_BASE_URL}/shopper/purchase/batch",
                    json=batch_payload,
                    headers=headers,
                )
                if resp.status_code in (200, 201):
                    success = True
                elif resp.status_code in (404, 405):
                    # Fall back to per-item calls only if batch endpoint is unavailable
                    failed_items = []
                    for item in self.cart_items:
                        for _ in range(item.quantity):
                            item_resp = client.post(
                                f"{API_BASE_URL}/shopper/purchase",
                                json={
                                    "product_id": item.product_id,
                                    "product_category_1": item.product_category_1,
                                    "product_category_2": item.product_category_2,
                                    "product_category_3": item.product_category_3,
                                },
                                headers=headers,
                            )
                            if item_resp.status_code not in (200, 201):
                                failed_items.append(item.name or item.product_id)
                                break
                    if not failed_items:
                        success = True
                    else:
                        self.checkout_error = f"Failed to checkout items: {', '.join(failed_items)}"
                else:
                    try:
                        err_detail = resp.json().get("detail", "Checkout failed.")
                    except Exception:
                        err_detail = f"Checkout failed with status {resp.status_code}."
                    self.checkout_error = str(err_detail)
        except Exception as e:
            self.checkout_error = f"Checkout network error: {str(e)}"

        if success:
            self.cart_items = []
            self.is_cart_open = False
            self.checkout_error = ""
            # Refresh history after checkout
            self.load_purchase_history()

    # ── Purchase history ────────────────────────────────────────────────
    def load_purchase_history(self):
        if not self.auth_token:
            return
        self.is_history_loading = True
        try:
            with httpx.Client(timeout=5.0) as client:
                resp = client.get(
                    f"{API_BASE_URL}/shopper/history",
                    headers={"Authorization": f"Bearer {self.auth_token}"}
                )
                if resp.status_code == 200:
                    raw_purchases = resp.json().get("purchases", [])
                    entries = []
                    for r in raw_purchases:
                        p_usd = float(r.get("predicted_usd", 0.0))
                        dt = str(r.get("purchased_at", ""))
                        date_str = dt[:10] if len(dt) >= 10 else dt
                        entries.append(PurchaseEntry(
                            id=int(r.get("id", 0)),
                            product_id=str(r.get("product_id", "")),
                            predicted_usd=p_usd,
                            price_display=f"${p_usd:.2f}",
                            purchased_at_display=date_str,
                        ))
                    self.purchase_history = entries
        except Exception:
            pass
        self.is_history_loading = False

    def toggle_history(self):
        self.show_history = not self.show_history
        if self.show_history and not self.purchase_history:
            self.load_purchase_history()

    # ── Owner dashboard ─────────────────────────────────────────────────
    def load_dashboard(self):
        self.is_dashboard_loading = True
        try:
            with httpx.Client(timeout=8.0) as client:
                s_resp = client.get(f"{API_BASE_URL}/analytics/summary")
                if s_resp.status_code == 200:
                    self.dashboard_summary = s_resp.json()

                # Pre-fetch all 4 dimensions into cache so tab clicks are instant
                dimensions = ["gender", "age", "city_category", "marital_status"]
                new_cache: Dict[str, List[DemographicRow]] = {}
                for dim in dimensions:
                    d_resp = client.get(f"{API_BASE_URL}/analytics/demographics/{dim}")
                    if d_resp.status_code == 200:
                        raw = d_resp.json()
                        total = sum(r.get("order_count", 0) for r in raw) or 1
                        rows = []
                        for r in raw:
                            cnt = int(r.get("order_count", 0))
                            avg_p_usd = float(r.get("avg_purchase", 0.0)) / INR_TO_USD_RATE
                            pct = round(cnt / total * 100, 1)
                            group_name = _format_demographic_group(dim, r.get("category", "-"))
                            rows.append(DemographicRow(
                                group=group_name,
                                total_orders=cnt,
                                avg_purchase_display=f"${avg_p_usd:.2f}",
                                pct_display=f"{pct:.1f}%",
                                pct_width=f"{min(100.0, pct):.1f}%",
                            ))
                        new_cache[dim] = rows
                self.dashboard_cache = new_cache
                if self.dashboard_dimension in new_cache:
                    self.dashboard_demographics = new_cache[self.dashboard_dimension]
        except Exception:
            pass
        finally:
            self.is_dashboard_loading = False

    def toggle_dashboard(self):
        self.show_dashboard = not self.show_dashboard
        if self.show_dashboard and not self.dashboard_cache:
            self.load_dashboard()

    def set_dashboard_dimension(self, dim: str):
        self.dashboard_dimension = dim
        if dim in self.dashboard_cache:
            self.dashboard_demographics = self.dashboard_cache[dim]
        else:
            self.load_dashboard()

    # ── Bot Assistant Handlers (Phase 1 Layer 1) ───────────────────────
    def set_is_bot_open(self, val: bool):
        self.is_bot_open = val

    def set_bot_input_text(self, val: str):
        self.bot_input_text = val

    def handle_bot_trigger_click(self):
        """Floating trigger click: performs JWT auth check before drawer access."""
        self.bot_auth_checked = True
        if not self.is_authenticated:
            self.bot_auth_warning = "Please sign in to unlock your personal AI shopping assistant and member deals."
            self.show_auth = True
            return False
        
        self.bot_auth_warning = ""
        self.is_bot_open = True
        if not self.bot_messages:
            name = self.user_name or "Shopper"
            persona = self.user_cluster_persona or "Vintage Enthusiast"
            self.bot_messages = [
                {
                    "role": "assistant",
                    "content": f"Welcome back, {name}! ({persona})\nHow can I assist you with our curated archives and personalized member pricing today?"
                }
            ]
        return True

    def open_bot_drawer(self):
        return self.handle_bot_trigger_click()

    def close_bot_drawer(self):
        self.is_bot_open = False

    def toggle_bot(self):
        if self.is_bot_open:
            self.is_bot_open = False
        else:
            self.handle_bot_trigger_click()

    def prompt_bot_login(self):
        self.is_bot_open = False
        self.show_auth = True

    def set_bot_mode(self, mode: str):
        """Switches between 'text' and 'voice' conversation modes."""
        self.bot_mode = mode

    def toggle_voice_listening(self):
        """Toggles real-time audio capture in voice mode."""
        self.bot_is_listening = not self.bot_is_listening
        if self.bot_is_listening:
            self.bot_messages.append({"role": "assistant", "content": "🎙️ Listening... Speak naturally to search archives or inquire about styles."})
        else:
            self.bot_messages.append({"role": "assistant", "content": "🎙️ Audio stream closed."})

    def _execute_bot_query(self, query: str):
        """Executes query through backend API endpoint (/bot/stream) over HTTP."""
        user_id = str(self.user_id) if self.user_id else "guest_user"
        session_id = f"session_{user_id}"

        try:
            resp = httpx.post(
                f"{API_BASE_URL}/bot/stream",
                json={
                    "query": query,
                    "user_id": user_id,
                    "session_id": session_id,
                    "mode": self.bot_mode,
                },
                timeout=30.0,
            )

            # Check 403 Security Strike Lockdown from Backend Middleware
            if resp.status_code == 403:
                try:
                    detail = resp.json().get("detail", "Security lockdown active.")
                except Exception:
                    detail = "Security lockdown active: Access temporarily restricted."
                self.bot_is_locked_out = True
                self.bot_lockout_message = detail
                self.bot_messages.append({"role": "assistant", "content": detail})
                return

            if resp.status_code != 200:
                self.bot_messages.append({
                    "role": "assistant",
                    "content": f"Service returned error code {resp.status_code}. Please try again shortly.",
                })
                return

            # Parse SSE chunks from HTTP stream response
            tokens: List[str] = []
            cards: List[Dict[str, Any]] = []

            for line in resp.text.split("\n"):
                line = line.strip()
                if not line.startswith("data:"):
                    continue
                data_str = line[5:].strip()
                if data_str == "[DONE]":
                    break
                try:
                    event = json.loads(data_str)
                    etype = event.get("type")
                    if etype == "token":
                        tokens.append(event.get("content", ""))
                    elif etype == "ui_card":
                        payload = event.get("payload", {})
                        if "cards" in payload:
                            cards = payload["cards"]
                        elif "products" in payload.get("data", {}):
                            cards = [
                                {
                                    "type": "PRODUCT_CARD",
                                    "product_id": p.get("product_id"),
                                    "name": p.get("name"),
                                    "price": p.get("discounted_price", p.get("price")),
                                    "image_url": p.get("image_url", f"/products/{p.get('product_id')}.jpg"),
                                    "badge": p.get("badge", "Curated Deal"),
                                }
                                for p in payload["data"]["products"]
                            ]
                except Exception:
                    continue

            final_text = "".join(tokens).strip() or "Here are the top catalog selections:"
            self.bot_messages.append({"role": "assistant", "content": final_text})
            self.bot_active_cards = cards

        except Exception as e:
            # Offline fallback if API server is not running locally
            chip_lower = query.lower()
            if "deal" in chip_lower:
                reply = "Here are our top recommended deals today: Check out the Artisan Paisley Silk Kimono Shirt ($49.90, 50% off) and the Midnight Ribbed Merino Wool Duster Coat ($119.90)!"
            elif "sale" in chip_lower:
                reply = "We currently have archival discounts across Jackets, Silks, and Knitwear! Explore our Sale badges for member-exclusive pricing."
            elif "under $50" in chip_lower or "50" in chip_lower:
                reply = "Top archival finds under $50: Vintage Brushed Flannel Camp Shirt ($44.90), Retro Gum-Sole Court Sneakers ($49.00), and Artisan Paisley Silk Kimono ($49.90)."
            elif "style" in chip_lower or "advisor" in chip_lower:
                persona = self.user_cluster_persona or "Modern Vintage"
                reply = f"Based on your {persona} profile, I recommend pairing tailored high-waist trousers with our hand-knitted cable cardigans and leather footwear."
            else:
                reply = f"Assistant offline: Unable to reach assistant service ({str(e)})."

            self.bot_messages.append({
                "role": "assistant",
                "content": reply,
            })



    def click_action_chip(self, chip: str):
        """Processes preset action chips (*'Top Deals Today'*, *'Sale Products'*)."""
        if not self.is_authenticated:
            self.bot_auth_warning = "Please sign in to use personalized shopping actions."
            self.show_auth = True
            return

        self.bot_messages.append({"role": "user", "content": chip})
        self._execute_bot_query(chip)

    def send_bot_message(self):
        """Dispatches typed message from input field to shopping DAG."""
        if not self.bot_input_text.strip():
            return
        msg = self.bot_input_text.strip()
        self.bot_input_text = ""
        self.bot_messages.append({"role": "user", "content": msg})
        self._execute_bot_query(msg)


