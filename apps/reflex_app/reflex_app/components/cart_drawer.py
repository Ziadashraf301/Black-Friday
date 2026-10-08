"""
Cart drawer with personalized member pricing, cluster persona badge,
purchase history panel, and owner dashboard link.
Fully typed with CartItem and PurchaseEntry.
"""
import reflex as rx
from reflex_app.state import ShoppingState, CartItem, PurchaseEntry


def cart_row(item: CartItem) -> rx.Component:
    return rx.box(
        rx.hstack(
            rx.image(
                src=item.image_url,
                alt=item.name,
                width="56px",
                height="56px",
                object_fit="cover",
                border_radius="6px",
            ),
            rx.vstack(
                rx.text(item.name, font_size="0.86rem", font_weight="700", color="#07281e", no_of_lines=1),
                rx.hstack(
                    rx.text("Size: " + item.size, font_size="0.72rem", color="#718278"),
                    rx.text("•", font_size="0.72rem", color="#b0beb6"),
                    rx.text("Qty: " + item.quantity.to_string(), font_size="0.72rem", color="#718278"),
                    spacing="2",
                ),
                rx.hstack(
                    rx.cond(
                        item.has_personalized,
                        rx.hstack(
                            rx.text(item.personalized_display, font_size="0.88rem", font_weight="700", color="#25a244"),
                            rx.text(item.price_display, font_size="0.72rem", color="#aab5ae", text_decoration="line-through"),
                            rx.box(
                                rx.text("Member Deal", font_size="0.62rem", color="#25a244", font_weight="700"),
                                background="#e8f9ed",
                                padding="0.08rem 0.35rem",
                                border_radius="3px",
                            ),
                            spacing="2",
                            align_items="center",
                        ),
                        rx.text(item.price_display, font_size="0.88rem", font_weight="700", color="#f59b38"),
                    ),
                    spacing="2",
                    align_items="center",
                ),
                align_items="flex-start",
                spacing="0",
                flex="1",
            ),
            rx.button(
                rx.icon(tag="trash-2", size=14, color="#a34848"),
                on_click=ShoppingState.remove_cart_item(item.key),
                background="transparent",
                border="none",
                cursor="pointer",
                padding="4px",
                _hover={"opacity": "0.7"},
            ),
            spacing="3",
            align_items="center",
            width="100%",
        ),
        background="#fbf6ec",
        border_radius="8px",
        padding="0.65rem 0.8rem",
        width="100%",
        margin_bottom="0.5rem",
    )


def member_panel() -> rx.Component:
    """Shows cluster persona info when logged in."""
    return rx.cond(
        ShoppingState.is_authenticated,
        rx.box(
            rx.hstack(
                rx.vstack(
                    rx.hstack(
                        rx.icon(tag="star", size=14, color="#f59b38"),
                        rx.text(
                            ShoppingState.user_persona_label,
                            font_size="0.78rem",
                            font_weight="700",
                            color="#07281e",
                        ),
                        spacing="1",
                        align_items="center",
                    ),
                    rx.text(
                        ShoppingState.welcome_name + " · " + ShoppingState.user_gender_label + " · Age " + ShoppingState.user_age,
                        font_size="0.72rem",
                        color="#496556",
                    ),
                    spacing="0",
                    align_items="flex-start",
                ),
                rx.hstack(
                    # History button
                    rx.button(
                        rx.hstack(rx.icon(tag="clock", size=13), rx.text("History", font_size="0.72rem"), spacing="1"),
                        on_click=ShoppingState.toggle_history,
                        background="#eaf4ee",
                        border="none",
                        border_radius="6px",
                        padding="0.25rem 0.5rem",
                        cursor="pointer",
                        color="#07281e",
                        _hover={"background": "#d2ead9"},
                    ),
                    # Dashboard button
                    rx.button(
                        rx.hstack(rx.icon(tag="bar-chart-2", size=13), rx.text("Dashboard", font_size="0.72rem"), spacing="1"),
                        on_click=ShoppingState.toggle_dashboard,
                        background="#fbf6ec",
                        border="none",
                        border_radius="6px",
                        padding="0.25rem 0.5rem",
                        cursor="pointer",
                        color="#07281e",
                        _hover={"background": "#f0e6cf"},
                    ),
                    spacing="1",
                ),
                justify_content="space-between",
                align_items="center",
                width="100%",
            ),
            background="#f0faf3",
            border="1px solid #c8e6d0",
            border_radius="8px",
            padding="0.6rem 0.8rem",
            width="100%",
        ),
    )


def history_row(p: PurchaseEntry) -> rx.Component:
    return rx.hstack(
        rx.text(p.product_id, font_size="0.75rem", color="#07281e", font_weight="600"),
        rx.text(p.price_display, font_size="0.75rem", color="#25a244", font_weight="700"),
        rx.text(p.purchased_at_display, font_size="0.7rem", color="#718278"),
        justify_content="space-between",
        width="100%",
        border_bottom="1px solid #eedec7",
        padding_bottom="0.3rem",
    )


def history_panel() -> rx.Component:
    return rx.cond(
        ShoppingState.show_history,
        rx.box(
            rx.vstack(
                rx.hstack(
                    rx.text("Purchase History", font_size="0.88rem", font_weight="700", color="#07281e"),
                    rx.text(
                        "Total: " + ShoppingState.total_spent_display,
                        font_size="0.75rem",
                        color="#496556",
                    ),
                    justify_content="space-between",
                    width="100%",
                ),
                rx.cond(
                    ShoppingState.is_history_loading,
                    rx.hstack(rx.spinner(size="2"), rx.text("Loading history...", font_size="0.82rem"), spacing="2"),
                    rx.cond(
                        ShoppingState.total_purchase_count > 0,
                        rx.vstack(
                            rx.foreach(ShoppingState.purchase_history, history_row),
                            width="100%",
                            spacing="1",
                            max_height="160px",
                            overflow_y="auto",
                        ),
                        rx.text("No past purchases yet.", font_size="0.82rem", color="#718278"),
                    ),
                ),
                spacing="2",
                width="100%",
            ),
            background="#ffffff",
            border="1px solid #eedec7",
            border_radius="8px",
            padding="0.75rem",
            width="100%",
        ),
    )


def cart_drawer() -> rx.Component:
    return rx.dialog.root(
        rx.dialog.content(
            rx.vstack(
                # Header
                rx.hstack(
                    rx.hstack(
                        rx.icon(tag="shopping-bag", size=20, color="#07281e"),
                        rx.text("Your Vintage Bag",
                                font_family="Georgia, 'Playfair Display', serif",
                                font_size="1.2rem", font_weight="700", color="#07281e"),
                        spacing="2", align_items="center",
                    ),
                    rx.dialog.close(
                        rx.button(
                            rx.icon(tag="x", size=18, color="#718278"),
                            on_click=ShoppingState.toggle_cart,
                            background="transparent", border="none", cursor="pointer",
                        ),
                    ),
                    justify_content="space-between", align_items="center",
                    width="100%", border_bottom="1px solid #eedec7", padding_bottom="0.8rem",
                ),
                # Member panel (cluster + history/dashboard links)
                member_panel(),
                # History panel
                history_panel(),
                # Item list
                rx.box(
                    rx.cond(
                        ShoppingState.cart_count > 0,
                        rx.vstack(rx.foreach(ShoppingState.cart_items, cart_row), width="100%", spacing="1"),
                        rx.vstack(
                            rx.icon(tag="shopping-bag", size=36, color="#b0beb6"),
                            rx.text("Your vintage bag is empty.", color="#718278", font_size="0.9rem"),
                            rx.text("Browse our curated archive to find your next piece.",
                                    font_size="0.8rem", color="#b0beb6", text_align="center"),
                            spacing="2", align_items="center", padding="2rem 0",
                        ),
                    ),
                    max_height="280px", overflow_y="auto", width="100%", padding_y="0.5rem",
                ),
                # Totals + checkout
                rx.cond(
                    ShoppingState.cart_count > 0,
                    rx.vstack(
                        rx.hstack(
                            rx.text("Catalog Total", font_size="0.85rem", color="#718278"),
                            rx.text(
                                ShoppingState.cart_subtotal_display,
                                font_size="0.85rem",
                                color="#718278",
                                text_decoration=rx.cond(ShoppingState.is_authenticated, "line-through", "none"),
                            ),
                            justify_content="space-between",
                            width="100%",
                        ),
                        rx.cond(
                            ShoppingState.is_authenticated,
                            rx.vstack(
                                rx.hstack(
                                    rx.hstack(
                                        rx.icon(tag="star", size=13, color="#25a244"),
                                        rx.text("Member Deal Total", font_size="0.9rem", color="#07281e", font_weight="700"),
                                        spacing="1",
                                    ),
                                    rx.text(
                                        ShoppingState.cart_personalized_total_display,
                                        font_size="1.15rem",
                                        font_weight="800",
                                        color="#25a244",
                                    ),
                                    justify_content="space-between",
                                    width="100%",
                                ),
                                rx.hstack(
                                    rx.text("You Save", font_size="0.75rem", color="#25a244", font_weight="600"),
                                    rx.text(ShoppingState.cart_savings_display, font_size="0.75rem", color="#25a244", font_weight="700"),
                                    justify_content="space-between",
                                    width="100%",
                                ),
                                spacing="1",
                                width="100%",
                            ),
                        ),
                        rx.hstack(
                            rx.text("Shipping", font_size="0.85rem", color="#718278"),
                            rx.text("FREE", font_size="0.85rem", color="#25a244", font_weight="700"),
                            justify_content="space-between",
                            width="100%",
                        ),
                        rx.cond(
                            ShoppingState.checkout_error != "",
                            rx.hstack(
                                rx.icon(tag="triangle-alert", size=15, color="#dc2626"),
                                rx.text(ShoppingState.checkout_error, font_size="0.78rem", color="#b91c1c", font_weight="500"),
                                background="#fef2f2",
                                border="1px solid #fecaca",
                                border_radius="6px",
                                padding="0.4rem 0.6rem",
                                spacing="2",
                                align_items="center",
                                width="100%",
                            ),
                        ),
                        rx.button(
                            rx.hstack(
                                rx.icon(tag="credit-card", size=16, color="#ffffff"),
                                rx.cond(
                                    ShoppingState.is_authenticated,
                                    rx.text("Checkout · " + ShoppingState.cart_personalized_total_display, font_weight="700", color="#ffffff"),
                                    rx.text("Log In & Checkout · " + ShoppingState.cart_subtotal_display, font_weight="700", color="#ffffff"),
                                ),
                                spacing="2",
                            ),
                            on_click=ShoppingState.checkout,
                            background="#07281e",
                            width="100%",
                            padding="0.8rem",
                            border_radius="8px",
                            cursor="pointer",
                            border="none",
                            margin_top="0.5rem",
                            _hover={"background": "#144837"},
                        ),
                        spacing="2",
                        width="100%",
                        border_top="1px solid #eedec7",
                        padding_top="0.8rem",
                    ),
                ),
                spacing="3",
                width="100%",
            ),
            max_width="440px",
            background="#ffffff",
            padding="1.5rem",
            border_radius="16px",
            box_shadow="0 20px 48px rgba(0,0,0,0.25)",
        ),
        open=ShoppingState.is_cart_open,
        on_open_change=ShoppingState.set_is_cart_open,
    )
