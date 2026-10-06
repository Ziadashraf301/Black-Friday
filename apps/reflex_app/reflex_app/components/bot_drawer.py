"""
Bot Assistant Drawer Component (Task P1-06).
Features:
  - Floating Trigger Button with pulsing badge & vintage styling
  - JWT Auth check gating personalized assistance
  - Preset Action Chips (*"Top Deals Today"*, *"Sale Products"*, *"Under $50"*, *"Style Advisor"*)
  - Interactive Conversation Pane
"""
import reflex as rx
from reflex_app.state import ShoppingState


def action_chip(label: str) -> rx.Component:
    """Renders an interactive preset action chip."""
    return rx.button(
        rx.hstack(
            rx.icon(tag="sparkles", size=13, color="#f59b38"),
            rx.text(label, font_size="0.75rem", font_weight="600"),
            spacing="1",
            align_items="center",
        ),
        on_click=ShoppingState.click_action_chip(label),
        background="#fbf6ec",
        color="#07281e",
        border="1px solid #eedec7",
        border_radius="9999px",
        padding="0.35rem 0.75rem",
        cursor="pointer",
        _hover={
            "background": "#07281e",
            "color": "#ffffff",
            "border_color": "#07281e",
            "transform": "translateY(-1px)",
        },
        transition="all 0.15s ease",
    )


def message_bubble(msg: dict) -> rx.Component:
    """Renders a single chat bubble."""
    is_user = msg["role"] == "user"
    return rx.box(
        rx.hstack(
            rx.cond(
                is_user,
                rx.fragment(),
                rx.box(
                    rx.icon(tag="bot", size=16, color="#ffffff"),
                    background="#07281e",
                    padding="6px",
                    border_radius="9999px",
                    flex_shrink="0",
                ),
            ),
            rx.box(
                rx.text(
                    msg["content"],
                    font_size="0.82rem",
                    color=rx.cond(is_user, "#ffffff", "#07281e"),
                    line_height="1.45",
                    white_space="pre-wrap",
                ),
                background=rx.cond(is_user, "#07281e", "#f5eee1"),
                padding="0.65rem 0.95rem",
                border_radius="14px",
                max_width="85%",
            ),
            justify_content=rx.cond(is_user, "flex-end", "flex-start"),
            align_items="flex-start",
            spacing="2",
            width="100%",
        ),
        width="100%",
        padding_y="0.25rem",
    )


def bot_auth_gate() -> rx.Component:
    """Shows when guest user needs to authenticate for personalized AI shopping."""
    return rx.vstack(
        rx.box(
            rx.icon(tag="shield-alert", size=32, color="#f59b38"),
            background="#fff5ea",
            padding="12px",
            border_radius="9999px",
            border="1px solid #f9dab3",
        ),
        rx.text("Member Authentication Required",
                font_family="Georgia, 'Playfair Display', serif",
                font_size="1.1rem", font_weight="700", color="#07281e"),
        rx.text(
            "Sign in to your member account to unlock personalized shopping, AI price quotes, and Apriori bundle styling.",
            font_size="0.8rem", color="#718278", text_align="center", max_width="320px",
        ),
        rx.button(
            rx.hstack(
                rx.icon(tag="log-in", size=16),
                rx.text("Sign In / Join Free", font_weight="600"),
                spacing="2",
            ),
            on_click=ShoppingState.prompt_bot_login,
            background="#07281e",
            color="#ffffff",
            padding="0.65rem 1.4rem",
            border_radius="8px",
            cursor="pointer",
            border="none",
            _hover={"background": "#144837"},
            margin_top="0.5rem",
        ),
        spacing="3",
        align_items="center",
        padding="2.5rem 1rem",
        width="100%",
    )


def bot_card_item(card: dict) -> rx.Component:
    """Renders a retrieved product card directly in the bot assistant drawer."""
    return rx.box(
        rx.hstack(
            rx.image(
                src=card["image_url"],
                width="50px",
                height="50px",
                border_radius="6px",
                object_fit="cover",
                border="1px solid #eedec7",
            ),
            rx.vstack(
                rx.hstack(
                    rx.text(card["name"], font_size="0.8rem", font_weight="700", color="#07281e", no_of_lines=1),
                    rx.badge(card["badge"], color_scheme="orange", size="1"),
                    spacing="1",
                    align_items="center",
                ),
                rx.hstack(
                    rx.text(f"${card['price']}", font_size="0.75rem", font_weight="700", color="#f59b38"),
                    rx.cond(
                        card["type"] == "BUNDLE_CARD",
                        rx.text(f"Bundle: {card.get('discount_pct')}", font_size="0.7rem", color="#25a244", font_weight="600"),
                        rx.fragment(),
                    ),
                    spacing="2",
                ),
                spacing="0",
                align_items="flex-start",
                flex="1",
            ),
            spacing="2",
            align_items="center",
            width="100%",
        ),
        background="#fbf6ec",
        padding="0.5rem 0.75rem",
        border_radius="8px",
        border="1px solid #eedec7",
        width="100%",
    )


def bot_drawer() -> rx.Component:
    """The interactive slide-over Bot Assistant Drawer with Text/Voice toggle."""
    return rx.dialog.root(
        rx.dialog.content(
            rx.vstack(
                # Drawer Header
                rx.hstack(
                    rx.hstack(
                        rx.box(
                            rx.icon(tag="sparkles", size=18, color="#07281e"),
                            background="#f59b38",
                            padding="6px",
                            border_radius="8px",
                        ),
                        rx.vstack(
                            rx.text("Vintage AI Concierge",
                                    font_family="Georgia, 'Playfair Display', serif",
                                    font_size="1.1rem", font_weight="700", color="#07281e"),
                            rx.text("Powered by Gemini 2.0 & Apriori Intelligence",
                                    font_size="0.72rem", color="#718278"),
                            spacing="0",
                            align_items="flex-start",
                        ),
                        spacing="2",
                        align_items="center",
                    ),
                    rx.dialog.close(
                        rx.button(
                            rx.icon(tag="x", size=18, color="#718278"),
                            on_click=ShoppingState.close_bot_drawer,
                            background="transparent",
                            border="none",
                            cursor="pointer",
                        ),
                    ),
                    justify_content="space-between",
                    align_items="center",
                    width="100%",
                    border_bottom="1px solid #eedec7",
                    padding_bottom="0.8rem",
                ),

                # Body: Auth Check or Chat
                rx.cond(
                    ShoppingState.is_authenticated,
                    # Authenticated Experience
                    rx.vstack(
                        # Mode Switcher (Text vs. Voice Live)
                        rx.hstack(
                            rx.button(
                                rx.hstack(
                                    rx.icon(tag="message-square", size=14),
                                    rx.text("Text Mode", font_size="0.75rem", font_weight="600"),
                                    spacing="1",
                                ),
                                on_click=ShoppingState.set_bot_mode("text"),
                                background=rx.cond(ShoppingState.bot_mode == "text", "#07281e", "#f5eee1"),
                                color=rx.cond(ShoppingState.bot_mode == "text", "#ffffff", "#07281e"),
                                border="none",
                                border_radius="6px",
                                padding="0.3rem 0.7rem",
                                cursor="pointer",
                            ),
                            rx.button(
                                rx.hstack(
                                    rx.icon(tag="mic", size=14),
                                    rx.text("Live Voice Mode", font_size="0.75rem", font_weight="600"),
                                    spacing="1",
                                ),
                                on_click=ShoppingState.set_bot_mode("voice"),
                                background=rx.cond(ShoppingState.bot_mode == "voice", "#f59b38", "#f5eee1"),
                                color=rx.cond(ShoppingState.bot_mode == "voice", "#07281e", "#07281e"),
                                border="none",
                                border_radius="6px",
                                padding="0.3rem 0.7rem",
                                cursor="pointer",
                            ),
                            spacing="2",
                            width="100%",
                        ),

                        # Security Strike Lockout Alert Banner
                        rx.cond(
                            ShoppingState.bot_is_locked_out,
                            rx.box(
                                rx.hstack(
                                    rx.icon(tag="shield-alert", size=18, color="#e63946"),
                                    rx.text(ShoppingState.bot_lockout_message, font_size="0.75rem", color="#721c24", font_weight="600"),
                                    spacing="2",
                                    align_items="center",
                                ),
                                background="#f8d7da",
                                border="1px solid #f5c6cb",
                                border_radius="8px",
                                padding="0.6rem 0.8rem",
                                width="100%",
                            ),
                            rx.fragment(),
                        ),

                        # Persona Ribbon
                        rx.box(
                            rx.hstack(
                                rx.icon(tag="user-check", size=14, color="#25a244"),
                                rx.text(
                                    "Connected as: " + ShoppingState.welcome_name + " • " + ShoppingState.user_persona_label,
                                    font_size="0.75rem", font_weight="600", color="#07281e",
                                ),
                                spacing="2",
                                align_items="center",
                            ),
                            background="#eef7f1",
                            border="1px solid #c9ebd4",
                            border_radius="6px",
                            padding="0.4rem 0.75rem",
                            width="100%",
                        ),

                        # Preset Action Chips
                        rx.vstack(
                            rx.text("Quick Actions & Deals", font_size="0.72rem", font_weight="700", color="#718278", text_transform="uppercase"),
                            rx.hstack(
                                rx.foreach(ShoppingState.bot_action_chips, action_chip),
                                spacing="2",
                                flex_wrap="wrap",
                            ),
                            width="100%",
                            spacing="1",
                        ),

                        # Conversation Pane
                        rx.box(
                            rx.vstack(
                                rx.foreach(ShoppingState.bot_messages, message_bubble),
                                width="100%",
                                spacing="2",
                            ),
                            max_height="260px",
                            min_height="160px",
                            overflow_y="auto",
                            width="100%",
                            padding_y="0.5rem",
                        ),

                        # Active Product Cards Carousel / List
                        rx.cond(
                            ShoppingState.bot_active_cards.length() > 0,
                            rx.vstack(
                                rx.text("Highlighted Catalog Selections", font_size="0.72rem", font_weight="700", color="#718278", text_transform="uppercase"),
                                rx.vstack(
                                    rx.foreach(ShoppingState.bot_active_cards, bot_card_item),
                                    spacing="2",
                                    width="100%",
                                    max_height="180px",
                                    overflow_y="auto",
                                ),
                                width="100%",
                                spacing="1",
                            ),
                            rx.fragment(),
                        ),

                        # Mode-Specific Input: Voice Mic or Text Input
                        rx.cond(
                            ShoppingState.bot_mode == "voice",
                            # Live Voice Controller
                            rx.box(
                                rx.vstack(
                                    rx.button(
                                        rx.hstack(
                                            rx.icon(tag="mic", size=24, color="#ffffff"),
                                            rx.text(
                                                rx.cond(ShoppingState.bot_is_listening, "Stop Streaming", "Start Voice Stream"),
                                                font_weight="700",
                                                font_size="0.85rem",
                                            ),
                                            spacing="2",
                                        ),
                                        on_click=ShoppingState.toggle_voice_listening,
                                        background=rx.cond(ShoppingState.bot_is_listening, "#e63946", "#07281e"),
                                        padding="0.75rem 1.5rem",
                                        border_radius="9999px",
                                        cursor="pointer",
                                    ),
                                    rx.text(
                                        rx.cond(
                                            ShoppingState.bot_is_listening,
                                            "🎙️ Streaming audio to Gemini Multimodal Live WS...",
                                            "Click above to speak naturally with Vintage AI Concierge",
                                        ),
                                        font_size="0.72rem",
                                        color="#718278",
                                    ),
                                    spacing="2",
                                    align_items="center",
                                    width="100%",
                                ),
                                background="#fbf6ec",
                                padding="1rem",
                                border_radius="10px",
                                border="1px dashed #eedec7",
                                width="100%",
                            ),
                            # Standard Text Input Pane
                            rx.hstack(
                                rx.el.input(
                                    placeholder="Ask about sizes, styles, or deals...",
                                    value=ShoppingState.bot_input_text,
                                    on_change=ShoppingState.set_bot_input_text,
                                    disabled=ShoppingState.bot_is_locked_out,
                                    style={
                                        "flex": "1",
                                        "padding": "0.55rem 0.85rem",
                                        "border": "1.5px solid #dde8e2",
                                        "border_radius": "8px",
                                        "font_size": "0.85rem",
                                        "outline": "none",
                                        "color": "#07281e",
                                        "background": "#fdfbf7",
                                    },
                                ),
                                rx.button(
                                    rx.icon(tag="send", size=16, color="#ffffff"),
                                    on_click=ShoppingState.send_bot_message,
                                    disabled=ShoppingState.bot_is_locked_out,
                                    background="#07281e",
                                    border="none",
                                    border_radius="8px",
                                    padding="0.55rem 0.85rem",
                                    cursor="pointer",
                                    _hover={"background": "#144837"},
                                ),
                                width="100%",
                                spacing="2",
                                border_top="1px solid #eedec7",
                                padding_top="0.8rem",
                            ),
                        ),
                        width="100%",
                        spacing="3",
                    ),
                    # Unauthenticated Fallback Gate
                    bot_auth_gate(),
                ),
                spacing="3",
                width="100%",
            ),
            max_width="450px",
            background="#ffffff",
            padding="1.5rem",
            border_radius="16px",
            box_shadow="0 24px 60px rgba(7,40,30,0.22)",
        ),
        open=ShoppingState.is_bot_open,
        on_open_change=ShoppingState.set_is_bot_open,
    )


def bot_trigger_button() -> rx.Component:
    """Floating Action Button fixed in bottom-right corner."""
    return rx.box(
        rx.button(
            rx.hstack(
                rx.icon(tag="bot", size=24, color="#f59b38"),
                rx.text("AI Bot", font_size="0.75rem", font_weight="700", color="#ffffff"),
                spacing="2",
                align_items="center",
            ),
            on_click=ShoppingState.handle_bot_trigger_click,
            background="#07281e",
            border="2px solid #f59b38",
            border_radius="9999px",
            padding="0.65rem 1.1rem",
            box_shadow="0 10px 30px rgba(7,40,30,0.35)",
            cursor="pointer",
            _hover={
                "background": "#144837",
                "transform": "scale(1.05)",
                "box_shadow": "0 14px 36px rgba(7,40,30,0.45)",
            },
            transition="all 0.2s cubic-bezier(0.4, 0, 0.2, 1)",
        ),
        position="fixed",
        bottom="24px",
        right="24px",
        z_index="999",
    )
