"""
Sidebar 'Shop By' accordion filter.
Uses 100% data-driven values matching curated_products.json fields:
Category, Gender, Brand, Style, and Season.
"""
import reflex as rx
from reflex_app.state import ShoppingState


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
                        category_chip("All"),
                        category_chip("Silks & Kimonos"),
                        category_chip("Jackets & Outerwear"),
                        category_chip("Footwear & Boots"),
                        category_chip("Dresses & Skirts"),
                        category_chip("Knitwear & Sweaters"),
                        category_chip("Coats & Trenches"),
                        category_chip("Leather & Outerwear"),
                        category_chip("Pants & Trousers"),
                        category_chip("Tops & Tunics"),
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
                        gender_chip("All"),
                        gender_chip("Women"),
                        gender_chip("Men"),
                        gender_chip("Unisex"),
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
                        brand_chip("All"),
                        brand_chip("Heritage Guild"),
                        brand_chip("Varsity Club"),
                        brand_chip("Cobbler Craft"),
                        brand_chip("Galway Knits"),
                        brand_chip("Aero Classics"),
                        brand_chip("Surplus Co."),
                        brand_chip("Sienna & Co."),
                        brand_chip("Nautical Archive"),
                        brand_chip("Byzantine Bloom"),
                        brand_chip("Atelier 1968"),
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
                        style_chip("All"),
                        style_chip("Boho Chic"),
                        style_chip("Bohemian Luxe"),
                        style_chip("Retro Sport"),
                        style_chip("Preppy Vintage"),
                        style_chip("Cozy Classic"),
                        style_chip("Rugged Aviator"),
                        style_chip("Utilitarian"),
                        style_chip("Modern Minimalist"),
                        style_chip("Maritime Classic"),
                        style_chip("Heritage Workwear"),
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
                        season_chip("All"),
                        season_chip("All-Season"),
                        season_chip("Autumn"),
                        season_chip("Winter"),
                        season_chip("Spring / Fall"),
                        season_chip("Winter / Fall"),
                        season_chip("Festive / Evening"),
                        season_chip("All-Weather"),
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
