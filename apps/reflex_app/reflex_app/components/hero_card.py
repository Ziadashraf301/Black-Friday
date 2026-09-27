"""
Featured Hero Card component on the right column matching the reference design.
Features the deep forest green backdrop, floating circular orange 'Sale' badge,
hanger photography, strikethrough & sale pricing, interactive size pills [S, M, L, XL],
wide vibrant orange 'Shop Now' CTA, and bottom circled chevron indicator.
"""
from typing import Dict, Any
import reflex as rx
from reflex_app.state import ShoppingState


def size_pill(size: str) -> rx.Component:
    is_active = ShoppingState.hero_selected_size == size
    return rx.box(
        rx.text(
            size,
            font_size="0.85rem",
            font_weight="700",
            color=rx.cond(is_active, "#ffffff", "#c2d5c9"),
        ),
        width="38px",
        height="34px",
        display="flex",
        align_items="center",
        justify_content="center",
        border_radius="8px",
        background=rx.cond(is_active, "#f59b38", "#123a2d"),
        border=rx.cond(is_active, "1px solid #f59b38", "1px solid #1a4a3b"),
        cursor="pointer",
        on_click=ShoppingState.set_hero_size(size),
        _hover={
            "background": rx.cond(is_active, "#f59b38", "#1a4f3e"),
            "transform": "scale(1.05)",
        },
        transition="all 0.15s ease",
    )


def hero_card() -> rx.Component:
    hero = ShoppingState.hero_product

    return rx.box(
        rx.vstack(
            # Top Header / Tags & Floating Circular 'Sale' Badge
            rx.box(
                # Floating Circular Orange 'Sale' Badge
                rx.box(
                    rx.text(
                        "Sale",
                        font_family="Georgia, 'Playfair Display', serif",
                        font_size="0.95rem",
                        font_weight="800",
                        color="#ffffff",
                        letter_spacing="0.04em",
                    ),
                    position="absolute",
                    top="-14px",
                    right="-14px",
                    width="56px",
                    height="56px",
                    border_radius="9999px",
                    background="#f59b38",
                    display="flex",
                    align_items="center",
                    justify_content="center",
                    box_shadow="0 6px 16px rgba(245, 155, 56, 0.45)",
                    z_index="10",
                ),
                # Inner Product Image Frame
                rx.box(
                    rx.image(
                        src="/products/P00025442.jpg",
                        alt="Vintage Paisley Silk Kimono Shirt",
                        width="100%",
                        height="260px",
                        object_fit="cover",
                        border_radius="10px",
                    ),
                    width="100%",
                    border_radius="10px",
                    overflow="hidden",
                    background="#ffffff",
                    box_shadow="0 8px 20px rgba(0,0,0,0.25)",
                    cursor="pointer",
                    on_click=ShoppingState.open_product_detail("P00025442"),
                ),
                position="relative",
                width="100%",
            ),
            # Product Title & Vintage Tagline
            rx.vstack(
                rx.text(
                    "Artisan Paisley Silk Kimono",
                    font_family="Georgia, 'Playfair Display', serif",
                    font_size="1.25rem",
                    font_weight="700",
                    color="#ffffff",
                    line_height="1.2",
                ),
                rx.text(
                    "Heritage 1970s emerald botanical archive robe",
                    font_size="0.82rem",
                    color="#a7c2b2",
                    line_height="1.3",
                ),
                spacing="1",
                align_items="flex-start",
                padding_top="1rem",
                width="100%",
            ),
            # Strikethrough & Bold Sale Price Display
            rx.hstack(
                rx.text(
                    "$99.90",
                    font_size="1.05rem",
                    color="#789384",
                    text_decoration="line-through",
                    font_weight="500",
                ),
                rx.text(
                    "$49.90",
                    font_size="1.6rem",
                    font_weight="800",
                    color="#f59b38",
                    letter_spacing="-0.02em",
                ),
                spacing="3",
                align_items="baseline",
                width="100%",
                padding_top="0.4rem",
            ),
            # Size Selector Pills [S, M, L, XL]
            rx.vstack(
                rx.text(
                    "Select Size:",
                    font_size="0.78rem",
                    font_weight="600",
                    color="#a7c2b2",
                    text_transform="uppercase",
                    letter_spacing="0.06em",
                ),
                rx.hstack(
                    size_pill("S"),
                    size_pill("M"),
                    size_pill("L"),
                    size_pill("XL"),
                    spacing="2",
                ),
                align_items="flex-start",
                spacing="2",
                padding_top="0.8rem",
                width="100%",
            ),
            # CTA: Wide Vibrant Orange 'Shop Now' Button
            rx.button(
                rx.hstack(
                    rx.text(
                        "Shop Now",
                        font_size="1.02rem",
                        font_weight="700",
                        color="#ffffff",
                        letter_spacing="0.02em",
                    ),
                    rx.icon(tag="arrow-right", size=18, color="#ffffff"),
                    spacing="2",
                    align_items="center",
                    justify_content="center",
                ),
                on_click=ShoppingState.add_hero_to_cart,
                width="100%",
                padding="0.8rem",
                background="#f59b38",
                border="none",
                border_radius="10px",
                cursor="pointer",
                margin_top="1.2rem",
                box_shadow="0 6px 18px rgba(245, 155, 56, 0.4)",
                _hover={
                    "background": "#e28624",
                    "transform": "translateY(-2px)",
                    "box_shadow": "0 8px 24px rgba(245, 155, 56, 0.5)",
                    "transition": "all 0.15s ease",
                },
            ),
            # Circled Down Chevron Indicator
            rx.box(
                rx.box(
                    rx.icon(tag="chevron-down", size=18, color="#a7c2b2"),
                    width="32px",
                    height="32px",
                    border_radius="9999px",
                    border="1.5px solid #1a4a3b",
                    display="flex",
                    align_items="center",
                    justify_content="center",
                    cursor="pointer",
                    on_click=ShoppingState.open_product_detail("P00025442"),
                    _hover={"border_color": "#f59b38", "color": "#f59b38"},
                ),
                display="flex",
                justify_content="center",
                width="100%",
                padding_top="1rem",
            ),
            spacing="0",
            width="100%",
        ),
        background="#0a2920",
        border_radius="18px",
        padding="1.6rem 1.4rem",
        box_shadow="0 14px 34px rgba(7, 40, 30, 0.35)",
        border="1px solid #174536",
        width=["100%", "100%", "320px", "340px"],
        position="relative",
    )
