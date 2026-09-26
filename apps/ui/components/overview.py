import streamlit as st
from apps.ui.api_client import APIClient
from apps.ui.components.eda_charts import render_all_eda_charts


def render_overview_tab(client: APIClient):
    """
    Renders the Executive Overview KPI dashboard and empirical demographic charts
    with embedded statistical hypothesis test badges.
    """
    st.title("📊 Executive Overview & Demographic Statistical Significance")
    st.markdown("Warehouse-scale KPI summary and read-only empirical distributions with embedded Welch's t-test and ANOVA results.")

    # 1. KPI Metric Cards
    eda_summary = client.get_analytics_summary() or {
        "total_orders": 550068,
        "total_users": 5891,
        "total_products": 3631,
        "avg_order_value": 9263.97,
        "total_revenue": 5095812740.0
    }

    c1, c2, c3, c4 = st.columns(4)
    c1.metric("Total Revenue", f"${eda_summary['total_revenue']:,.2f}")
    c2.metric("Total Transactions", f"{eda_summary['total_orders']:,}")
    c3.metric("Unique Customers", f"{eda_summary['total_users']:,}")
    c4.metric("Avg Order Value", f"${eda_summary['avg_order_value']:,.2f}")

    st.markdown("---")

    # 2. Render all 5 demographic charts with embedded statistical significance
    render_all_eda_charts(client)
