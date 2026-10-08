"""
Product card component matching the 2x2 grid in the vintage mockup.
Features the orange 'Newest' pill badge, editorial photo, serif typography,
discounted pricing, and circular orange arrow CTA button.
"""
from typing import Dict, Any
import reflex as rx
from reflex_app.state import ShoppingState


def product_card(product: Dict[str, Any]) -> rx.Component:
    return rx.box(
        rx.vstack(
            # Top Image Container with 'Newest' Badge
            rx.box(
                # 'Newest' Pill Badge
                rx.box(
                    rx.text(
                        "Newest",
                        font_size="0.68rem",
                        font_weight="700",
                        color="#ffffff",
                        letter_spacing="0.04em",
                    ),
                    position="absolute",
                    top="10px",
                    left="10px",
                    background="#f59b38",
                    padding="0.2rem 0.65rem",
                    border_radius="9999px",
                    z_index="2",
                    box_shadow="0 2px 4px rgba(245, 155, 56, 0.4)",
                ),
                # Product Image
                rx.image(
                    src=product["image_url"],
                    alt=product["name"],
                    width="100%",
                    height="190px",
                    object_fit="cover",
                    border_radius="8px",
                    background="#fcfbf9",
                ),
                width="100%",
                position="relative",
                overflow="hidden",
                border_radius="8px",
            ),
            # Content & CTA
            rx.hstack(
                # Title and Price
                rx.vstack(
                    rx.text(
                        product["name"],
                        font_family="Georgia, 'Playfair Display', serif",
                        font_size="0.95rem",
                        font_weight="700",
                        color="#07281e",
                        line_height="1.25",
                        no_of_lines=1,
                    ),
                    rx.hstack(
                        rx.text(
                            product["category_name"],
                            font_size="0.75rem",
                            color="#718278",
                        ),
                        rx.text("•", font_size="0.75rem", color="#b0beb6"),
                        rx.text(
                            product["discounted_price"].to_string()
                            if hasattr(product["discounted_price"], "to_string")
                            else f"${float(product['discounted_price']):.2f}"
                            if isinstance(product.get("discounted_price"), (int, float))
                            else str(product.get("discounted_price", "")),
                            font_size="0.88rem",
                            font_weight="700",
                            color="#07281e",
                        ),
                        spacing="2",
                        align_items="center",
                    ),
                    align_items="flex-start",
                    spacing="1",
                    flex="1",
                ),
                # Circular Orange Arrow Action Indicator
                rx.box(
                    rx.icon(tag="arrow-right", size=16, color="#ffffff"),
                    width="34px",
                    height="34px",
                    border_radius="9999px",
                    background="#f59b38",
                    display="flex",
                    align_items="center",
                    justify_content="center",
                    box_shadow="0 3px 8px rgba(245, 155, 56, 0.35)",
                    transition="all 0.15s ease",
                ),
                width="100%",
                justify_content="space-between",
                align_items="center",
                padding_top="0.65rem",
            ),
            spacing="0",
            width="100%",
        ),
        background="#ffffff",
        border_radius="14px",
        padding="0.85rem",
        box_shadow="0 6px 18px rgba(10, 41, 32, 0.06)",
        border="1px solid rgba(220, 210, 195, 0.6)",
        transition="all 0.2s cubic-bezier(0.16, 1, 0.3, 1)",
        _hover={
            "transform": "translateY(-4px)",
            "box_shadow": "0 12px 24px rgba(10, 41, 32, 0.12)",
        },
        cursor="pointer",
        on_click=ShoppingState.open_product_detail(product["product_id"]),
    )


def product_grid() -> rx.Component:
    """Renders 2x2 grid matching the reference layout."""
    return rx.box(
        rx.cond(
            ShoppingState.filtered_products.length() > 0,
            rx.grid(
                rx.foreach(ShoppingState.filtered_products, product_card),
                columns=rx.breakpoints(initial="1", sm="2"),
                spacing="4",
                width="100%",
            ),
            rx.vstack(
                rx.text("No products match the selected criteria.", color="#718278", font_size="0.9rem"),
                rx.button("Reset Filters", on_click=ShoppingState.reset_filters, background="#07281e", color="#fff", size="2"),
                spacing="3",
                align_items="center",
                padding="3rem",
            ),
        ),
        flex="1",
        width="100%",
    )
