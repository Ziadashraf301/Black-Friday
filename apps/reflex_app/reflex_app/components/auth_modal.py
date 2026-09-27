"""
Auth modal — clean, elegant login/signup experience.
Shows automatically when user clicks 'Log In' in the header.
No technical jargon exposed to end users.
"""
import reflex as rx
from reflex_app.state import ShoppingState

OCCUPATION_LABELS = [
    (0, "Student"),
    (1, "Technology"),
    (2, "Healthcare"),
    (3, "Management"),
    (4, "Finance"),
    (5, "Legal"),
    (6, "Retail / Sales"),
    (7, "Engineering"),
    (8, "Trades"),
    (9, "Education"),
    (10, "Government"),
    (11, "Hospitality"),
    (12, "Agriculture"),
    (13, "Media"),
    (14, "Transport"),
    (15, "Arts & Design"),
    (16, "Freelance"),
    (17, "Real Estate"),
    (18, "Retired"),
    (19, "Homemaker"),
    (20, "Other"),
]


def input_field(label: str, placeholder: str, value, on_change, type_: str = "text") -> rx.Component:
    return rx.vstack(
        rx.text(label, font_size="0.78rem", font_weight="600", color="#496556"),
        rx.el.input(
            placeholder=placeholder,
            value=value,
            on_change=on_change,
            type=type_,
            style={
                "width": "100%",
                "padding": "0.55rem 0.8rem",
                "border": "1.5px solid #dde8e2",
                "border_radius": "8px",
                "font_size": "0.9rem",
                "color": "#07281e",
                "background": "#f9fdf9",
                "outline": "none",
                "transition": "border-color 0.15s ease",
            },
        ),
        spacing="1",
        width="100%",
        align_items="flex-start",
    )


def select_field(label: str, value, on_change, options: list) -> rx.Component:
    return rx.vstack(
        rx.text(label, font_size="0.78rem", font_weight="600", color="#496556"),
        rx.select.root(
            rx.select.trigger(
                placeholder="Select...",
                style={
                    "width": "100%",
                    "border": "1.5px solid #dde8e2",
                    "border_radius": "8px",
                    "background": "#f9fdf9",
                    "color": "#07281e",
                    "font_size": "0.88rem",
                    "cursor": "pointer",
                },
            ),
            rx.select.content(
                *[rx.select.item(lbl, value=str(val)) for val, lbl in options],
                background="#ffffff",
            ),
            value=value,
            on_change=on_change,
        ),
        spacing="1",
        width="100%",
        align_items="flex-start",
    )


def login_panel() -> rx.Component:
    return rx.vstack(
        rx.text(
            "Welcome Back",
            font_family="Georgia, 'Playfair Display', serif",
            font_size="1.35rem",
            font_weight="700",
            color="#07281e",
        ),
        rx.text(
            "Sign in to access your personalised pricing and vintage picks.",
            font_size="0.85rem",
            color="#718278",
            padding_bottom="0.5rem",
        ),
        input_field("Email", "you@example.com", ShoppingState.login_email, ShoppingState.set_login_email, "email"),
        input_field("Password", "••••••••", ShoppingState.login_password, ShoppingState.set_login_password, "password"),
        rx.cond(
            ShoppingState.auth_error != "",
            rx.hstack(
                rx.icon(tag="triangle-alert", size=18, color="#dc2626"),
                rx.text(ShoppingState.auth_error, font_size="0.84rem", color="#b91c1c", font_weight="600"),
                background="#fef2f2",
                border="1.5px solid #f87171",
                border_radius="8px",
                padding="0.65rem 0.9rem",
                spacing="2",
                align_items="center",
                width="100%",
            ),
        ),
        rx.button(
            rx.cond(
                ShoppingState.auth_loading,
                rx.hstack(rx.spinner(size="2"), rx.text("Signing in..."), spacing="2"),
                rx.text("Sign In →", font_weight="700", color="#ffffff"),
            ),
            on_click=ShoppingState.do_login,
            width="100%",
            padding="0.75rem",
            background="#07281e",
            border="none",
            border_radius="8px",
            cursor="pointer",
            _hover={"background": "#144837"},
        ),
        rx.hstack(
            rx.text("New to the store?", font_size="0.82rem", color="#718278"),
            rx.text(
                "Create account",
                font_size="0.82rem",
                color="#f59b38",
                font_weight="600",
                cursor="pointer",
                text_decoration="underline",
                on_click=ShoppingState.set_auth_tab("signup"),
            ),
            spacing="2",
            justify_content="center",
        ),
        spacing="3",
        width="100%",
        align_items="stretch",
    )


def signup_panel() -> rx.Component:
    return rx.vstack(
        rx.text(
            "Join THE Second Hand STORE",
            font_family="Georgia, 'Playfair Display', serif",
            font_size="1.25rem",
            font_weight="700",
            color="#07281e",
        ),
        rx.text(
            "Create your profile to unlock member pricing and curated picks.",
            font_size="0.82rem",
            color="#718278",
            padding_bottom="0.3rem",
        ),
        rx.grid(
            input_field("Full Name", "Jane Doe", ShoppingState.signup_name, ShoppingState.set_signup_name),
            input_field("Email", "you@example.com", ShoppingState.signup_email, ShoppingState.set_signup_email, "email"),
            columns="2",
            spacing="3",
            width="100%",
        ),
        input_field("Password", "••••••••", ShoppingState.signup_password, ShoppingState.set_signup_password, "password"),
        rx.grid(
            select_field("Gender", ShoppingState.signup_gender, ShoppingState.set_signup_gender,
                         [("M", "Male"), ("F", "Female")]),
            select_field("Age Bracket", ShoppingState.signup_age, ShoppingState.set_signup_age,
                         [("0-17", "Under 18"), ("18-25", "18–25"), ("26-35", "26–35"),
                          ("36-45", "36–45"), ("46-50", "46–50"), ("51-55", "51–55"), ("55+", "55+")]),
            select_field("City Tier", ShoppingState.signup_city, ShoppingState.set_signup_city,
                         [("A", "Metro City (A)"), ("B", "Large City (B)"), ("C", "Small City (C)")]),
            columns="3",
            spacing="3",
            width="100%",
        ),
        rx.cond(
            ShoppingState.auth_error != "",
            rx.hstack(
                rx.icon(tag="triangle-alert", size=18, color="#dc2626"),
                rx.text(ShoppingState.auth_error, font_size="0.84rem", color="#b91c1c", font_weight="600"),
                background="#fef2f2",
                border="1.5px solid #f87171",
                border_radius="8px",
                padding="0.65rem 0.9rem",
                spacing="2",
                align_items="center",
                width="100%",
            ),
        ),
        rx.button(
            rx.cond(
                ShoppingState.auth_loading,
                rx.hstack(rx.spinner(size="2"), rx.text("Creating account..."), spacing="2"),
                rx.text("Create Account →", font_weight="700", color="#ffffff"),
            ),
            on_click=ShoppingState.do_signup,
            width="100%",
            padding="0.75rem",
            background="#f59b38",
            border="none",
            border_radius="8px",
            cursor="pointer",
            _hover={"background": "#e28624"},
        ),
        rx.hstack(
            rx.text("Already have an account?", font_size="0.82rem", color="#718278"),
            rx.text(
                "Sign in",
                font_size="0.82rem",
                color="#07281e",
                font_weight="600",
                cursor="pointer",
                text_decoration="underline",
                on_click=ShoppingState.set_auth_tab("login"),
            ),
            spacing="2",
            justify_content="center",
        ),
        spacing="3",
        width="100%",
        align_items="stretch",
        max_height="70vh",
        overflow_y="auto",
    )


def auth_modal() -> rx.Component:
    return rx.dialog.root(
        rx.dialog.content(
            rx.vstack(
                # Header row
                rx.hstack(
                    rx.hstack(
                        rx.icon(tag="recycle", size=18, color="#25a244"),
                        rx.text(
                            "THE Second Hand STORE",
                            font_family="Georgia, serif",
                            font_size="1rem",
                            font_weight="700",
                            color="#07281e",
                        ),
                        spacing="2",
                        align_items="center",
                    ),
                    rx.dialog.close(
                        rx.button(
                            rx.icon(tag="x", size=18, color="#718278"),
                            on_click=ShoppingState.close_auth,
                            background="transparent",
                            border="none",
                            cursor="pointer",
                        ),
                    ),
                    justify_content="space-between",
                    width="100%",
                    border_bottom="1px solid #edf3ef",
                    padding_bottom="0.8rem",
                ),
                # Body — login or signup panel
                rx.cond(
                    ShoppingState.auth_tab == "login",
                    login_panel(),
                    signup_panel(),
                ),
                spacing="3",
                width="100%",
            ),
            max_width="480px",
            background="#ffffff",
            padding="1.5rem",
            border_radius="16px",
            box_shadow="0 20px 48px rgba(0,0,0,0.22)",
        ),
        open=ShoppingState.show_auth,
        on_open_change=ShoppingState.set_show_auth,
    )
