"""
Quick-View Modal — product detail, ONNX pricing, and recommendation navigation.
Clicking a recommendation switches the modal directly to that product.
Fully typed with RecProduct for 100% Reflex compiler compatibility.
"""
import reflex as rx
from reflex_app.state import ShoppingState, RecProduct


def rec_card(item: RecProduct) -> rx.Component:
    """A clickable recommendation card. Clicking anywhere switches directly to that product."""
    return rx.box(
        rx.hstack(
            rx.image(
                src=item.image_url,
                alt=item.name,
                width="60px",
                height="60px",
                object_fit="cover",
                border_radius="6px",
                flex_shrink="0",
            ),
            rx.vstack(
                rx.text(
                    item.name,
                    font_size="0.85rem",
                    font_weight="700",
                    color="#07281e",
                    no_of_lines=1,
                ),
                rx.hstack(
                    rx.text(item.price_display, font_size="0.82rem", font_weight="700", color="#f59b38"),
                    rx.box(
                        rx.text(item.badge_label, font_size="0.68rem", color="#07281e", font_weight="600"),
                        background="#eaf4ee",
                        padding="0.1rem 0.4rem",
                        border_radius="4px",
                    ),
                    spacing="2",
                    align_items="center",
                ),
                align_items="flex-start",
                spacing="1",
                flex="1",
            ),
            rx.box(
                rx.icon(tag="arrow-right", size=14, color="#07281e"),
                width="28px",
                height="28px",
                border_radius="9999px",
                background="#fbf6ec",
                border="1px solid #eedec7",
                display="flex",
                align_items="center",
                justify_content="center",
                flex_shrink="0",
            ),
            spacing="3",
            align_items="center",
            width="100%",
        ),
        background="#ffffff",
        border="1.5px solid #eedec7",
        border_radius="8px",
        padding="0.5rem 0.7rem",
        width="100%",
        cursor="pointer",
        on_click=ShoppingState.navigate_to_rec(item.product_id),
        transition="all 0.15s ease",
        _hover={"border_color": "#07281e", "box_shadow": "0 2px 8px rgba(7,40,30,0.12)", "transform": "translateX(2px)"},
    )


def size_button_modal(size: str) -> rx.Component:
    return rx.button(
        size,
        on_click=ShoppingState.set_active_size(size),
        background=rx.cond(ShoppingState.active_selected_size == size, "#07281e", "transparent"),
        color=rx.cond(ShoppingState.active_selected_size == size, "#ffffff", "#07281e"),
        border=rx.cond(ShoppingState.active_selected_size == size, "none", "1.5px solid #c9dbd2"),
        border_radius="6px",
        padding="0.4rem 0.8rem",
        font_weight="600",
        font_size="0.82rem",
        cursor="pointer",
        transition="all 0.15s ease",
        _hover={"background": "#07281e", "color": "#ffffff"},
    )


def pricing_box() -> rx.Component:
    return rx.box(
        rx.hstack(
            rx.vstack(
                rx.cond(
                    ShoppingState.is_authenticated,
                    rx.vstack(
                        rx.hstack(
                            rx.icon(tag="star", size=14, color="#25a244"),
                            rx.text("Your Exclusive Member Price", font_size="0.75rem", font_weight="700", color="#25a244"),
                            spacing="1", align_items="center",
                        ),
                        rx.cond(
                            ShoppingState.is_predicting_price,
                            rx.hstack(rx.spinner(size="2"), rx.text("Calculating personalized deal...", font_size="0.85rem", color="#718278"), spacing="2"),
                            rx.hstack(
                                rx.text(
                                    ShoppingState.active_estimated_price_display,
                                    font_size="1.6rem",
                                    font_weight="800",
                                    color="#25a244",
                                ),
                                rx.text(
                                    ShoppingState.active_sale_price_display,
                                    font_size="1.0rem",
                                    color="#aab5ae",
                                    text_decoration="line-through",
                                ),
                                spacing="3", align_items="baseline",
                            ),
                        ),
                        rx.text(
                            "Special Member Deal",
                            font_size="0.7rem", color="#496556",
                        ),
                        spacing="0", align_items="flex-start",
                    ),
                    rx.vstack(
                        rx.text("Catalog Price", font_size="0.72rem", font_weight="600", color="#496556"),
                        rx.text(
                            ShoppingState.active_sale_price_display,
                            font_size="1.6rem",
                            font_weight="800",
                            color="#f59b38",
                        ),
                        rx.text(
                            "Log in to unlock personalized member discounts",
                            font_size="0.72rem",
                            color="#718278",
                            font_style="italic",
                        ),
                        spacing="0",
                        align_items="flex-start",
                    ),
                ),
                align_items="flex-start",
                spacing="0",
            ),
            rx.vstack(
                rx.text(ShoppingState.active_original_price_display, font_size="0.72rem", color="#718278"),
                rx.cond(
                    ShoppingState.active_product_badge != "",
                    rx.box(
                        rx.text(ShoppingState.active_product_badge, font_size="0.72rem", font_weight="700", color="#f59b38"),
                        background="rgba(245,155,56,0.12)",
                        border="1px solid #f59b38",
                        border_radius="4px",
                        padding="0.15rem 0.5rem",
                    ),
                ),
                align_items="flex-end",
                spacing="1",
            ),
            justify_content="space-between",
            align_items="flex-end",
            width="100%",
        ),
        background="#fbf6ec",
        border="1px solid #eedec7",
        border_radius="8px",
        padding="0.75rem 1rem",
        width="100%",
        margin_top="0.5rem",
    )


def quick_view_modal() -> rx.Component:
    return rx.dialog.root(
        rx.dialog.content(
            rx.vstack(
                # Header
                rx.hstack(
                    rx.text(
                        "Product Details",
                        font_family="Georgia, 'Playfair Display', serif",
                        font_size="1.2rem",
                        font_weight="700",
                        color="#07281e",
                    ),
                    rx.dialog.close(
                        rx.button(
                            rx.icon(tag="x", size=18, color="#718278"),
                            on_click=ShoppingState.close_quick_view,
                            background="transparent",
                            border="none",
                            cursor="pointer",
                            _hover={"color": "#07281e"},
                        ),
                    ),
                    justify_content="space-between",
                    align_items="center",
                    width="100%",
                    border_bottom="1px solid #eedec7",
                    padding_bottom="0.8rem",
                ),
                # Body
                rx.hstack(
                    # Left: product image + meta
                    rx.vstack(
                        rx.image(
                            src=ShoppingState.active_product_image,
                            alt=ShoppingState.active_product_name,
                            width="260px",
                            height="300px",
                            object_fit="cover",
                            border_radius="10px",
                            box_shadow="0 4px 12px rgba(0,0,0,0.1)",
                        ),
                        rx.hstack(
                            rx.box(
                                rx.text(ShoppingState.active_product_id, font_size="0.72rem", font_weight="700", color="#07281e"),
                                background="#fbf6ec",
                                padding="0.2rem 0.6rem",
                                border_radius="4px",
                            ),
                            rx.box(
                                rx.text(
                                    ShoppingState.active_orders_display,
                                    font_size="0.72rem",
                                    color="#496556",
                                    font_weight="500",
                                ),
                                background="#eaf4ee",
                                padding="0.2rem 0.6rem",
                                border_radius="4px",
                            ),
                            spacing="2",
                            flex_wrap="wrap",
                        ),
                        width="260px",
                        flex_shrink="0",
                        spacing="3",
                    ),
                    # Right: details + recommendations
                    rx.vstack(
                        rx.text(
                            ShoppingState.active_product_name,
                            font_family="Georgia, 'Playfair Display', serif",
                            font_size="1.35rem",
                            font_weight="700",
                            color="#07281e",
                            line_height="1.2",
                        ),
                        rx.text(
                            ShoppingState.active_product_tagline,
                            font_size="0.85rem",
                            color="#718278",
                            font_style="italic",
                        ),
                        rx.text(
                            ShoppingState.active_product_desc,
                            font_size="0.88rem",
                            color="#384f42",
                            line_height="1.5",
                        ),
                        # Price box
                        pricing_box(),
                        # Size selector
                        rx.vstack(
                            rx.text("Select Size", font_size="0.78rem", font_weight="700", color="#496556", letter_spacing="0.05em"),
                            rx.hstack(
                                rx.foreach(
                                    ShoppingState.active_product_sizes,
                                    size_button_modal,
                                ),
                                spacing="2",
                            ),
                            align_items="flex-start",
                            spacing="2",
                            margin_top="0.4rem",
                        ),
                        # Apriori bundles
                        rx.cond(
                            ShoppingState.active_apriori_bundles_list.length() > 0,
                            rx.vstack(
                                rx.hstack(
                                    rx.icon(tag="sparkles", size=14, color="#f59b38"),
                                    rx.text(
                                        "Frequently Bought Together",
                                        font_size="0.82rem",
                                        font_weight="700",
                                        color="#07281e",
                                    ),
                                    spacing="2",
                                    align_items="center",
                                ),
                                rx.foreach(ShoppingState.active_apriori_bundles_list, rec_card),
                                width="100%",
                                spacing="2",
                                padding_top="0.4rem",
                            ),
                        ),
                        # Item2Vec similars
                        rx.cond(
                            ShoppingState.active_item2vec_similars_list.length() > 0,
                            rx.vstack(
                                rx.hstack(
                                    rx.icon(tag="layers", size=14, color="#2563eb"),
                                    rx.text(
                                        "Similar Archival Pieces",
                                        font_size="0.82rem",
                                        font_weight="700",
                                        color="#07281e",
                                    ),
                                    spacing="2",
                                    align_items="center",
                                ),
                                rx.foreach(ShoppingState.active_item2vec_similars_list, rec_card),
                                width="100%",
                                spacing="2",
                                padding_top="0.2rem",
                            ),
                        ),
                        # Add to Bag button
                        rx.button(
                            rx.hstack(
                                rx.icon(tag="shopping-bag", size=16, color="#ffffff"),
                                rx.text("Add to Bag", font_weight="700", color="#ffffff"),
                                spacing="2",
                            ),
                            on_click=ShoppingState.add_active_to_cart,
                            background="#07281e",
                            width="100%",
                            padding="0.75rem",
                            border_radius="8px",
                            margin_top="0.5rem",
                            cursor="pointer",
                            border="none",
                            _hover={"background": "#144837", "transform": "scale(1.01)"},
                        ),
                        width="100%",
                        spacing="2",
                        align_items="flex-start",
                        flex="1",
                        overflow_y="auto",
                        max_height="75vh",
                    ),
                    spacing="5",
                    align_items="flex-start",
                    width="100%",
                    padding_top="0.8rem",
                ),
                spacing="2",
                width="100%",
            ),
            max_width="820px",
            background="#ffffff",
            padding="1.5rem",
            border_radius="16px",
            box_shadow="0 20px 48px rgba(0,0,0,0.25)",
        ),
        open=ShoppingState.is_quick_view_open,
        on_open_change=ShoppingState.set_is_quick_view_open,
    )
