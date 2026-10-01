"""
Main application page for 'THE Second Hand STORE' Reflex frontend.
Faithfully recreates the visual design, typography, warm creamy vintage canvas (#fbf6ec),
dark forest green header (#07281e), Shop By accordion, 2x2 product grid, and featured hero card.
"""
import reflex as rx

from reflex_app.state import ShoppingState
from reflex_app.components.header import header
from reflex_app.components.sidebar import sidebar
from reflex_app.components.product_card import product_grid
from reflex_app.components.hero_card import hero_card
from reflex_app.components.quick_view_modal import quick_view_modal
from reflex_app.components.cart_drawer import cart_drawer
from reflex_app.components.auth_modal import auth_modal
from reflex_app.components.dashboard_modal import dashboard_modal
from reflex_app.components.bot_drawer import bot_drawer, bot_trigger_button



def decorative_elements() -> rx.Component:
    """Subtle vintage background geometries matching the reference mockup."""
    return rx.box(
        # Bottom-left vintage architectural arch
        rx.box(
            position="fixed",
            bottom="0",
            left="0",
            width="220px",
            height="220px",
            background="#f3e8d6",
            border_top_right_radius="220px",
            z_index="0",
            pointer_events="none",
            opacity="0.45",
        ),
        # Right edge decorative accent
        rx.box(
            position="fixed",
            top="45%",
            right="-30px",
            width="80px",
            height="80px",
            border_radius="9999px",
            background="#e5d5be",
            z_index="0",
            pointer_events="none",
            opacity="0.3",
        ),
    )


def index() -> rx.Component:
    return rx.box(
        decorative_elements(),
        # Fixed / Sticky Navigation Header
        header(),
        # Main Canvas
        rx.box(
            rx.box(
                rx.hstack(
                    # 1. Left "Shop By" Accordion Sidebar
                    sidebar(),
                    # 2. Center 2x2 Product Grid
                    product_grid(),
                    # 3. Right Featured Hero Card
                    hero_card(),
                    align_items="flex-start",
                    spacing="6",
                    width="100%",
                    display=["flex", "flex", "flex", "flex"],
                    flex_direction=["column", "column", "row", "row"],
                ),
                max_width="1400px",
                margin="0 auto",
                padding=["1.5rem 1rem", "2rem 1.5rem", "2.5rem 2rem", "3rem 2.5rem"],
                position="relative",
                z_index="1",
            ),
            width="100%",
            min_height="calc(100vh - 65px)",
            background="#fbf6ec",
        ),
        # Interactive Modals & Drawers
        quick_view_modal(),
        cart_drawer(),
        auth_modal(),
        dashboard_modal(),
        bot_drawer(),
        bot_trigger_button(),
        width="100%",
        min_height="100vh",
        background="#fbf6ec",
        font_family="'Inter', -apple-system, BlinkMacSystemFont, sans-serif",
    )


app = rx.App(
    head_components=[
        rx.el.link(
            rel="preconnect",
            href="https://fonts.googleapis.com",
        ),
        rx.el.link(
            rel="preconnect",
            href="https://fonts.gstatic.com",
            crossorigin="anonymous",
        ),
        rx.el.link(
            rel="stylesheet",
            href="https://fonts.googleapis.com/css2?family=Playfair+Display:ital,wght@0,600;0,700;0,800;1,600&family=Inter:wght@400;500;600;700&display=swap",
        ),
    ],
)

app.add_page(
    index,
    route="/",
    title="THE Second Hand STORE | Curated Vintage Archives & Smart Bundles",
    description="High-performance vintage fashion catalog powered by Apriori bundle association rules and ONNX price intelligence.",
    on_load=ShoppingState.load_catalog,
)
