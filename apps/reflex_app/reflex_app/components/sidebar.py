"""
Sidebar 'Shop By' accordion filter.
Uses 100% data-driven values matching curated_products.json fields:
Category, Gender, Brand, Style, and Season.
"""
from typing import List, Dict, Any
import reflex as rx
from reflex_app.state import ShoppingState


def extract_filter_options(products: List[Dict[str, Any]], field: str) -> List[str]:
    """Pure function extracting sorted unique non-empty filter options from products, with 'All' first."""
    if not products:
        return ["All"]
    values = set()
    for p in products:
        val = p.get(field)
        if val is not None:
            s_val = str(val).strip()
            if s_val and s_val != "All":
                values.add(s_val)
    return ["All"] + sorted(list(values))


def accordion_header(title: str, section_key: str) -> rx.Component:
    is_open = ShoppingState.open_accordion == section_key
    return rx.hstack(
        rx.hstack(
            rx.box(
                width="7px", height="7px", border_radius="9999px", background="#f59b38",
            ),
            rx.text(title, font_size="0.95rem", font_weight="600", color="#07281e", letter_spacing="0.02em"),
            spacing="2", align_items="center",
        ),
        rx.box(
            rx.icon(tag=rx.cond(is_open, "chevron-up", "chevron-down"), size=14, color="#07281e"),
            width="22px", height="22px", border_radius="9999px",
            border="1.5px solid #07281e", display="flex",
            align_items="center", justify_content="center",
        ),
        justify_content="space-between", align_items="center", width="100%",
        padding="0.6rem 0", cursor="pointer",
        on_click=ShoppingState.toggle_accordion(section_key),
        _hover={"opacity": "0.85"},
    )


def chip(label: str, is_selected: bool, on_click) -> rx.Component:
    return rx.box(
        rx.text(label, font_size="0.80rem",
                font_weight=rx.cond(is_selected, "600", "400"),
                color=rx.cond(is_selected, "#ffffff", "#07281e")),
        padding="0.22rem 0.6rem", border_radius="6px",
        background=rx.cond(is_selected, "#07281e", "#f3ede1"),
        cursor="pointer", on_click=on_click,
        _hover={"background": rx.cond(is_selected, "#07281e", "#eae0d0")},
        transition="all 0.15s ease",
    )


def category_chip(label: str) -> rx.Component:
    return chip(label, ShoppingState.selected_category == label,
                ShoppingState.set_filter_category(label))


def gender_chip(label: str) -> rx.Component:
    return chip(label, ShoppingState.selected_gender == label,
                ShoppingState.set_filter_gender(label))


def brand_chip(label: str) -> rx.Component:
    return chip(label, ShoppingState.selected_brand == label,
                ShoppingState.set_filter_brand(label))


def style_chip(label: str) -> rx.Component:
    return chip(label, ShoppingState.selected_style == label,
                ShoppingState.set_filter_style(label))


def season_chip(label: str) -> rx.Component:
    return chip(label, ShoppingState.selected_season == label,
                ShoppingState.set_filter_season(label))


def section(header, content) -> rx.Component:
    return rx.vstack(
        header, content,
        width="100%", border_bottom="1px solid #eedec7", spacing="0",
    )


def sidebar() -> rx.Component:
    return rx.box(
        # Header
        rx.vstack(
            rx.text("Shop By", font_family="Georgia, 'Playfair Display', serif",
                    font_size="1.45rem", font_weight="700", color="#07281e", line_height="1.2"),
            rx.box(width="48px", height="3.5px", background="#07281e",
                   border_radius="2px", margin_top="0.2rem", margin_bottom="0.8rem"),
            align_items="flex-start", spacing="1",
        ),
        rx.vstack(
            # 1. Categories
            section(
                accordion_header("Category", "category"),
                rx.cond(
                    ShoppingState.open_accordion == "category",
                    rx.hstack(
                        rx.foreach(ShoppingState.available_categories, category_chip),
                        flex_wrap="wrap", spacing="2", padding_bottom="0.5rem",
                    ),
                ),
            ),
            # 2. Gender
            section(
                accordion_header("Gender", "gender"),
                rx.cond(
                    ShoppingState.open_accordion == "gender",
                    rx.hstack(
                        rx.foreach(ShoppingState.available_genders, gender_chip),
                        flex_wrap="wrap", spacing="2", padding_bottom="0.5rem",
                    ),
                ),
            ),
            # 3. Brands
            section(
                accordion_header("Brands", "brand"),
                rx.cond(
                    ShoppingState.open_accordion == "brand",
                    rx.hstack(
                        rx.foreach(ShoppingState.available_brands, brand_chip),
                        flex_wrap="wrap", spacing="2", padding_bottom="0.5rem",
                    ),
                ),
            ),
            # 4. Style
            section(
                accordion_header("Style", "style"),
                rx.cond(
                    ShoppingState.open_accordion == "style",
                    rx.hstack(
                        rx.foreach(ShoppingState.available_styles, style_chip),
                        flex_wrap="wrap", spacing="2", padding_bottom="0.5rem",
                    ),
                ),
            ),
            # 5. Season
            section(
                accordion_header("Season", "season"),
                rx.cond(
                    ShoppingState.open_accordion == "season",
                    rx.hstack(
                        rx.foreach(ShoppingState.available_seasons, season_chip),
                        flex_wrap="wrap", spacing="2", padding_bottom="0.5rem",
                    ),
                ),
            ),
            width="100%", spacing="1",
        ),
        rx.button(
            "Reset All Filters",
            on_click=ShoppingState.reset_filters,
            background="transparent", color="#07281e",
            border="1px dashed #07281e", font_size="0.82rem", font_weight="600",
            padding="0.35rem 0.75rem", border_radius="6px",
            margin_top="1.2rem", width="100%", cursor="pointer",
            _hover={"background": "#eedec7"},
        ),
        width=["100%", "100%", "230px", "260px"],
        padding_right=["0", "0", "1.5rem", "2rem"],
    )
