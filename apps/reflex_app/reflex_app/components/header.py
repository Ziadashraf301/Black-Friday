"""
Header navigation — dark forest green, brand logo, nav links,
pill search, dynamic cart counter, and contextual Log In / member persona button.
"""
import reflex as rx
from reflex_app.state import ShoppingState


def header_action_button(text: str, on_click) -> rx.Component:
    return rx.button(
        text,
        on_click=on_click,
        background="transparent",
        color="#e3eee7",
        font_size="0.90rem",
        font_weight="500",
        border="none",
        cursor="pointer",
        padding="0.3rem 0.6rem",
        border_radius="6px",
        _hover={"color": "#f59b38", "background": "rgba(255,255,255,0.06)"},
        transition="all 0.15s ease",
    )


def header() -> rx.Component:
    return rx.box(
        rx.hstack(
            # Brand Logo
            rx.hstack(
                rx.box(rx.icon(tag="recycle", size=22, color="#25a244"), display="flex", align_items="center"),
                rx.vstack(
                    rx.text("THE Second Hand", font_family="Georgia, serif", font_size="1.05rem",
                            font_weight="700", color="#ffffff", line_height="1.1", letter_spacing="0.02em"),
                    rx.text("STORE", font_family="Georgia, serif", font_size="0.75rem",
                            font_weight="600", color="#f59b38", letter_spacing="0.25em", line_height="1"),
                    spacing="0", align_items="flex-start",
                ),
                spacing="2", align_items="center", cursor="pointer",
                on_click=ShoppingState.reset_filters,
            ),
            # Nav Links (wired to filter actions)
            rx.hstack(
                header_action_button("All Archive", ShoppingState.reset_filters),
                header_action_button("Women's", ShoppingState.set_filter_gender("Women")),
                header_action_button("Men's", ShoppingState.set_filter_gender("Men")),
                header_action_button("Unisex", ShoppingState.set_filter_gender("Unisex")),
                spacing="2",
                align_items="center",
                display=["none", "none", "flex", "flex"],
            ),
            # Right side: search + analytics + auth + cart
            rx.hstack(
                # Pill Search
                rx.hstack(
                    rx.icon(tag="search", size=15, color="#8a9990"),
                    rx.input(
                        placeholder="Search...",
                        value=ShoppingState.search_query,
                        on_change=ShoppingState.set_search_query,
                        border="none",
                        background="transparent",
                        outline="none",
                        font_size="0.82rem",
                        color="#1b382b",
                        width=["80px", "105px", "125px", "145px"],
                        _focus={"box_shadow": "none", "border": "none"},
                    ),
                    background="#ffffff",
                    border_radius="9999px",
                    padding="0.25rem 0.65rem",
                    align_items="center",
                    spacing="1",
                    box_shadow="0 2px 4px rgba(0,0,0,0.08)",
                    flex_shrink="1",
                ),
                # Owner Analytics Dashboard Button
                rx.button(
                    rx.hstack(
                        rx.icon(tag="bar-chart-2", size=13),
                        rx.text("Analytics", font_size="0.82rem", font_weight="600"),
                        spacing="1",
                        align_items="center",
                    ),
                    on_click=ShoppingState.toggle_dashboard,
                    background="rgba(245,155,56,0.18)",
                    color="#f59b38",
                    border="1px solid rgba(245,155,56,0.35)",
                    border_radius="9999px",
                    padding="0.28rem 0.75rem",
                    cursor="pointer",
                    white_space="nowrap",
                    flex_shrink="0",
                    _hover={"background": "rgba(245,155,56,0.30)", "color": "#ffffff"},
                    transition="all 0.15s ease",
                ),
                # Auth / Member button — contextual
                rx.cond(
                    ShoppingState.is_authenticated,
                    # Logged in — show cluster persona badge + welcome name + logout
                    rx.hstack(
                        rx.box(
                            rx.hstack(
                                rx.icon(tag="star", size=13, color="#f59b38"),
                                rx.text(ShoppingState.user_persona_label, font_size="0.75rem", font_weight="700", color="#07281e"),
                                spacing="1",
                                align_items="center",
                            ),
                            background="#fbf6ec",
                            border="1px solid #eedec7",
                            border_radius="9999px",
                            padding="0.25rem 0.65rem",
                            cursor="pointer",
                            on_click=ShoppingState.toggle_cart,
                            flex_shrink="0",
                        ),
                        rx.box(
                            rx.text(ShoppingState.welcome_name, font_size="0.82rem", font_weight="600", color="#07281e"),
                            background="#e2f5e9",
                            border_radius="9999px",
                            padding="0.28rem 0.75rem",
                            flex_shrink="0",
                        ),
                        rx.button(
                            rx.icon(tag="log-out", size=16, color="#e3eee7"),
                            on_click=ShoppingState.do_logout,
                            background="transparent",
                            border="none",
                            cursor="pointer",
                            title="Sign Out",
                            _hover={"color": "#f59b38"},
                            flex_shrink="0",
                        ),
                        spacing="2",
                        align_items="center",
                        flex_shrink="0",
                    ),
                    # Logged out — show Log In button
                    rx.button(
                        rx.hstack(
                            rx.icon(tag="user", size=15, color="#07281e"),
                            rx.text("Log In", font_size="0.88rem", font_weight="600", color="#07281e"),
                            spacing="1",
                        ),
                        on_click=ShoppingState.open_auth("login"),
                        background="#ffffff",
                        border_radius="9999px",
                        padding="0.3rem 0.85rem",
                        border="none",
                        cursor="pointer",
                        box_shadow="0 2px 4px rgba(0,0,0,0.08)",
                        _hover={"background": "#fbf6ec"},
                        flex_shrink="0",
                    ),
                ),
                # Cart pill
                rx.button(
                    rx.hstack(
                        rx.icon(tag="shopping-bag", size=16, color="#07281e"),
                        rx.text("Cart", font_size="0.88rem", font_weight="600", color="#07281e"),
                        rx.box(
                            rx.text(ShoppingState.cart_count.to_string(), font_size="0.78rem", font_weight="700", color="#ffffff"),
                            background="#f59b38",
                            border_radius="9999px",
                            padding="0.1rem 0.45rem",
                            line_height="1",
                        ),
                        spacing="2",
                        align_items="center",
                    ),
                    on_click=ShoppingState.toggle_cart,
                    background="#ffffff",
                    border_radius="9999px",
                    padding="0.3rem 0.85rem",
                    border="none",
                    cursor="pointer",
                    box_shadow="0 2px 4px rgba(0,0,0,0.08)",
                    _hover={"background": "#fbf6ec"},
                    flex_shrink="0",
                ),
                spacing="2",
                align_items="center",
                flex_shrink="0",
            ),
            justify_content="space-between",
            align_items="center",
            width="100%",
            max_width="1400px",
            margin="0 auto",
        ),
        background="#07281e",
        padding="0.75rem 2rem",
        position="sticky",
        top="0",
        z_index="50",
        box_shadow="0 2px 12px rgba(7,40,30,0.25)",
        width="100%",
    )
