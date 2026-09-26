import sys
from pathlib import Path

# Ensure project root is in sys.path when running via Streamlit
root_dir = Path(__file__).resolve().parent.parent.parent
if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))

import streamlit as st
import os
from apps.ui.api_client import APIClient
from apps.ui.components.overview import render_overview_tab
from apps.ui.components.shopper import render_shopper_experience

# Page configuration
st.set_page_config(
    page_title="Black Friday Platform",
    page_icon="🛍️",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Initialize API Client and sync token
api_base_url = os.getenv("API_BASE_URL", "http://localhost:8000")
client = APIClient(base_url=api_base_url)
if "shopper_token" in st.session_state and st.session_state["shopper_token"]:
    client.set_token(st.session_state["shopper_token"])

# Sidebar Header & Role Switcher
st.sidebar.title("🛍️ Black Friday Store")
st.sidebar.caption("Retail Operations & Customer Shopping Portal")

role_mode = st.sidebar.radio(
    "Select Portal View:",
    [
        "👔 Business Owner (Executive Analytics)",
        "🛍️ Shopper Experience (Store & Deals)"
    ],
    index=0
)

st.sidebar.markdown("---")

# Service Status Check via APIClient
health_data = client.get_health()
if health_data and health_data.get("status") == "healthy":
    st.sidebar.success("🟢 System Online & Ready")
else:
    st.sidebar.warning("🟡 System Status: Connecting...")

# Route to selected portal
if role_mode == "🛍️ Shopper Experience (Store & Deals)":
    render_shopper_experience(client)
else:
    render_overview_tab(client)
