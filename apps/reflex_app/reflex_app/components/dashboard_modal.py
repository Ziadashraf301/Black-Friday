"""
Owner/Business Dashboard modal.
Shows executive KPIs from the analytics API: total orders, revenue, AOV,
user count, and demographics breakdown.
Uses typed DemographicRow rx.Base objects for 100% Reflex compiler compatibility.
"""
import reflex as rx
from reflex_app.state import ShoppingState, DemographicRow


def kpi_card(label: str, value: str, icon: str, color: str = "#07281e") -> rx.Component:
    return rx.box(
        rx.vstack(
            rx.hstack(
                rx.icon(tag=icon, size=15, color=color),
                rx.text(label, font_size="0.7rem", color="#718278", font_weight="500"),
                spacing="1", align_items="center",
            ),
            rx.text(value, font_size="1.3rem", font_weight="800", color=color),
            spacing="1", align_items="flex-start",
        ),
        background="#ffffff", border="1px solid #e4ede8",
        border_radius="10px", padding="0.7rem 0.9rem",
        flex="1", min_width="110px",
        box_shadow="0 2px 6px rgba(0,0,0,0.05)",
    )


def demo_row(row: DemographicRow) -> rx.Component:
    return rx.hstack(
        rx.text(row.group, font_size="0.78rem", font_weight="600", color="#07281e", width="100px"),
        rx.text(row.total_orders.to_string(), font_size="0.78rem", color="#496556", width="80px"),
        rx.text(row.avg_purchase_display, font_size="0.78rem", color="#f59b38", font_weight="700", width="80px"),
        rx.box(
            rx.box(
                height="8px", background="#07281e", border_radius="4px",
                width=row.pct_width,
            ),
            background="#e4ede8", border_radius="4px", height="8px", flex="1",
        ),
        rx.text(row.pct_display, font_size="0.72rem", color="#718278", width="50px", text_align="right"),
        spacing="3", align_items="center", width="100%",
        border_bottom="1px solid #f3ede1", padding_bottom="0.3rem",
    )


def dashboard_modal() -> rx.Component:
    return rx.dialog.root(
        rx.dialog.content(
            rx.vstack(
                # Header
                rx.hstack(
                    rx.hstack(
                        rx.icon(tag="bar-chart-2", size=20, color="#07281e"),
                        rx.text(
                            "Store Analytics Dashboard",
                            font_family="Georgia, 'Playfair Display', serif",
                            font_size="1.2rem", font_weight="700", color="#07281e",
                        ),
                        spacing="2", align_items="center",
                    ),
                    rx.dialog.close(
                        rx.button(
                            rx.icon(tag="x", size=18, color="#718278"),
                            on_click=ShoppingState.toggle_dashboard,
                            background="transparent", border="none", cursor="pointer",
                        ),
                    ),
                    justify_content="space-between", align_items="center",
                    width="100%", border_bottom="1px solid #eedec7", padding_bottom="0.8rem",
                ),
                # Body
                rx.cond(
                    ShoppingState.is_dashboard_loading,
                    rx.hstack(rx.spinner(size="3"), rx.text("Loading store metrics..."), spacing="3", padding="2rem"),
                    rx.vstack(
                        # KPI row
                        rx.hstack(
                            kpi_card("Total Orders", ShoppingState.dashboard_orders_display, "shopping-cart", "#07281e"),
                            kpi_card("Revenue", ShoppingState.dashboard_revenue_display, "dollar-sign", "#f59b38"),
                            kpi_card("Customers", ShoppingState.dashboard_users_display, "users", "#25a244"),
                            kpi_card("Products", ShoppingState.dashboard_products_display, "package", "#2563eb"),
                            kpi_card("Avg Order", ShoppingState.dashboard_aov_display, "trending-up", "#8b5cf6"),
                            spacing="3", flex_wrap="wrap", width="100%",
                        ),
                        # Demographics
                        rx.vstack(
                            rx.hstack(
                                rx.text("Demographics Breakdown", font_size="0.88rem", font_weight="700", color="#07281e"),
                                rx.hstack(
                                    *[
                                        rx.button(
                                            dim.replace("_", " ").title(),
                                            on_click=ShoppingState.set_dashboard_dimension(dim),
                                            disabled=ShoppingState.is_dashboard_loading,
                                            background=rx.cond(ShoppingState.dashboard_dimension == dim, "#07281e", "#f3ede1"),
                                            color=rx.cond(ShoppingState.dashboard_dimension == dim, "#ffffff", "#07281e"),
                                            border="none", border_radius="6px",
                                            padding="0.2rem 0.55rem", font_size="0.75rem", cursor="pointer",
                                            opacity=rx.cond(ShoppingState.is_dashboard_loading, "0.5", "1.0"),
                                        )
                                        for dim in ["gender", "age", "city_category", "marital_status"]
                                    ],
                                    spacing="1",
                                ),
                                justify_content="space-between", align_items="center", width="100%",
                            ),
                            rx.hstack(
                                rx.text("Group", font_size="0.7rem", font_weight="700", color="#718278", width="100px"),
                                rx.text("Orders", font_size="0.7rem", font_weight="700", color="#718278", width="80px"),
                                rx.text("Avg Spend", font_size="0.7rem", font_weight="700", color="#718278", width="80px"),
                                rx.text("Distribution Share", font_size="0.7rem", font_weight="700", color="#718278", flex="1"),
                                rx.text("Pct", font_size="0.7rem", font_weight="700", color="#718278", width="50px", text_align="right"),
                                spacing="3", width="100%",
                            ),
                            rx.box(
                                rx.foreach(
                                    ShoppingState.dashboard_demographics,
                                    demo_row,
                                ),
                                width="100%", max_height="200px", overflow_y="auto",
                            ),
                            width="100%", spacing="2",
                            background="#fbf6ec", border_radius="8px", padding="0.75rem", margin_top="0.5rem",
                        ),
                        width="100%", spacing="3",
                    ),
                ),
                spacing="3", width="100%",
            ),
            max_width="800px", background="#ffffff",
            padding="1.5rem", border_radius="16px",
            box_shadow="0 20px 48px rgba(0,0,0,0.25)",
        ),
        open=ShoppingState.show_dashboard,
        on_open_change=ShoppingState.set_show_dashboard,
    )
