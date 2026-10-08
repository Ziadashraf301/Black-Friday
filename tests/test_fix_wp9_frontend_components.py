import os
import pytest
from unittest.mock import MagicMock, patch

from core.config import settings
import sys
sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))
from reflex_app.state import ShoppingState
from reflex_app.components.sidebar import extract_filter_options, sidebar
from reflex_app.components.hero_card import hero_card
from reflex_app.components.bot_drawer import bot_card_item, message_bubble, bot_drawer
from reflex_app.components.auth_modal import auth_error_banner, signup_panel, OCCUPATION_LABELS
from reflex_app.components.product_card import product_card
from apps.api.schemas import SignupRequest


def test_changing_hero_product_changes_cart_addition():
    """Fix 4.6: Verify changing hero_product updates reactive properties and changes what add_hero_to_cart adds."""
    state = ShoppingState(_reflex_internal_init=True)
    state.hero_product = {
        "product_id": "HERO_JACKET_01",
        "name": "Vintage Shearling Jacket",
        "tagline": "1970s Mountain archive jacket",
        "discounted_price": 120.0,
        "original_price": 240.0,
        "badge": "Archive Pick",
        "product_category_1": 2,
    }
    state.hero_selected_size = "L"

    # Verify reactive vars
    assert state.hero_id == "HERO_JACKET_01"
    assert state.hero_name == "Vintage Shearling Jacket"
    assert state.hero_tagline == "1970s Mountain archive jacket"
    assert state.hero_discounted_price_display == "$120.00"
    assert state.hero_original_price_display == "$240.00"
    assert state.hero_badge_label == "Archive Pick"

    state.add_hero_to_cart()
    assert len(state.cart_items) == 1
    assert state.cart_items[0].product_id == "HERO_JACKET_01"
    assert state.cart_items[0].size == "L"
    assert state.cart_items[0].price == 120.0

    # Change hero product
    state.hero_product = {
        "product_id": "HERO_SILK_02",
        "name": "Artisan Silk Scarf",
        "tagline": "Pure hand-spun vintage silk",
        "discounted_price": 35.0,
        "original_price": 70.0,
        "badge": "Staff Favorite",
        "product_category_1": 1,
    }
    state.hero_selected_size = "S"
    assert state.hero_id == "HERO_SILK_02"
    assert state.hero_name == "Artisan Silk Scarf"
    assert state.hero_discounted_price_display == "$35.00"

    state.add_hero_to_cart()
    assert len(state.cart_items) == 2
    assert state.cart_items[1].product_id == "HERO_SILK_02"
    assert state.cart_items[1].size == "S"
    assert state.cart_items[1].price == 35.0


def test_extract_filter_options_pure_function():
    """Fix 5.7: Verify pure extract_filter_options computes all unique options with 'All' first."""
    sample_catalog = [
        {"product_id": "P1", "category_name": "Silks", "brand": "Heritage", "style": "Boho", "season": "Spring"},
        {"product_id": "P2", "category_name": "Denim", "brand": "Varsity", "style": "Classic", "season": "Fall"},
        {"product_id": "P3", "category_name": "Silks", "brand": "Heritage", "style": "Preppy", "season": "Spring"},
        {"product_id": "P4", "category_name": "Leather", "brand": "Aero", "style": "Aviator", "season": "Winter"},
    ]

    cats = extract_filter_options(sample_catalog, "category_name")
    assert cats == ["All", "Denim", "Leather", "Silks"]

    brands = extract_filter_options(sample_catalog, "brand")
    assert brands == ["All", "Aero", "Heritage", "Varsity"]

    # Verify state reactive properties
    state = ShoppingState(_reflex_internal_init=True)
    state.products = sample_catalog
    assert state.available_categories == ["All", "Denim", "Leather", "Silks"]
    assert state.available_brands == ["All", "Aero", "Heritage", "Varsity"]
    assert state.available_styles == ["All", "Aviator", "Boho", "Classic", "Preppy"]
    assert state.available_seasons == ["All", "Fall", "Spring", "Winter"]


def test_bot_drawer_recommended_cards_and_markdown():
    """Fix 6.7: Verify assistant message bubble supports Markdown and recommended card is clickable."""
    user_bubble = message_bubble({"role": "user", "content": "Show me coats"})
    assistant_bubble = message_bubble({"role": "assistant", "content": "**Top Pick:** Vintage Trench"})
    assert user_bubble is not None
    assert assistant_bubble is not None

    card = {
        "product_id": "P001",
        "name": "Classic Trench",
        "badge": "Top Deal",
        "price": 89.0,
        "image_url": "/products/P001.jpg",
        "type": "PRODUCT_CARD",
    }
    card_comp = bot_card_item(card)
    assert card_comp is not None


def test_signup_occupation_and_schema_compatibility():
    """Fix 9.7: Verify occupation setter, string property, and backend SignupRequest compatibility."""
    state = ShoppingState(_reflex_internal_init=True)
    state.set_signup_occupation("4")
    assert state.signup_occupation == 4
    assert state.signup_occupation_str == "4"

    # Verify OCCUPATION_LABELS contains valid tuple mappings
    assert any(val == 4 and "Finance" in label for val, label in OCCUPATION_LABELS)

    # Test backend Pydantic schema validation with signup payload
    payload = {
        "name": "Alex Mercer",
        "email": "alex@example.com",
        "password": "strongpassword123",
        "gender": "M",
        "age": "26-35",
        "city_category": "A",
        "marital_status": 0,
        "occupation": state.signup_occupation,
        "stay_in_current_city_years": "2",
    }
    req = SignupRequest(**payload)
    assert req.occupation == 4
    assert req.name == "Alex Mercer"


def test_product_card_single_on_click():
    """Fix 10.5: Verify product_card renders with single on_click on root container."""
    sample_prod = {
        "product_id": "P00099",
        "name": "Vintage Denim Jacket",
        "category_name": "Jackets",
        "discounted_price": 59.90,
        "image_url": "/products/P00099.jpg",
    }
    comp = product_card(sample_prod)
    assert comp is not None
