import streamlit as st
import pandas as pd
from typing import Dict, Any, List, Optional
from apps.ui.api_client import APIClient

# Fallback catalog products if warehouse catalog is initializing
FALLBACK_CATALOG = [
    {"product_id": "P00110742", "order_count": 1612, "pagerank_score": 0.1754},
    {"product_id": "P00025442", "order_count": 1420, "pagerank_score": 0.1432},
    {"product_id": "P00184942", "order_count": 1180, "pagerank_score": 0.1205},
    {"product_id": "P00057642", "order_count": 1055, "pagerank_score": 0.0984},
    {"product_id": "P00145242", "order_count": 940, "pagerank_score": 0.0872},
    {"product_id": "P00254942", "order_count": 890, "pagerank_score": 0.0763},
]

OCCUPATION_MAP = {
    0: "Student / Academic",
    1: "Technology / Software",
    2: "Healthcare / Medical",
    3: "Executive / Management",
    4: "Finance / Banking",
    5: "Legal Services",
    6: "Retail / Sales",
    7: "Engineering",
    8: "Construction / Trades",
    9: "Education / Teacher",
    10: "Government / Public",
    11: "Hospitality / Tourism",
    12: "Agriculture / Forestry",
    13: "Media / Entertainment",
    14: "Transportation / Logistics",
    15: "Arts & Design",
    16: "Self-Employed / Freelance",
    17: "Real Estate",
    18: "Retired",
    19: "Homemaker",
    20: "Other / General",
}


def _render_auth_screen(client: APIClient):
    """Renders the Shopper Sign-Up and Log-In interface."""
    st.markdown("### 🛍️ Shopper Experience Portal")
    st.info("Log in or create an account to browse products, view tailored recommendations, and enjoy member pricing.")

    tab_login, tab_signup = st.tabs(["🔑 Log In", "📝 Create Account"])

    # ---------------- LOGIN TAB ----------------
    with tab_login:
        st.subheader("Welcome Back")
        with st.form("shopper_login_form"):
            login_email = st.text_input("Email Address", placeholder="e.g. shopper@example.com")
            login_password = st.text_input("Password", type="password")
            submitted = st.form_submit_button("Log In", width="stretch")

            if submitted:
                if not login_email or not login_password:
                    st.error("Please enter both email and password.")
                else:
                    with st.spinner("Signing in..."):
                        resp = client.login(login_email.strip(), login_password)
                    if resp and "_error" not in resp and "access_token" in resp:
                        token = resp["access_token"]
                        st.session_state["shopper_token"] = token
                        client.set_token(token)
                        st.session_state["shopper_user"] = {
                            "user_id": resp.get("user_id"),
                            "name": resp.get("name"),
                        }
                        st.success(f"Welcome back, {resp.get('name', 'Shopper')}!")
                        st.rerun()
                    else:
                        err_msg = resp.get("_error") if isinstance(resp, dict) else "Invalid credentials."
                        st.error(f"Login failed: {err_msg}")

    # ---------------- SIGNUP TAB ----------------
    with tab_signup:
        st.subheader("Join the Black Friday Experience")
        st.caption("Create your shopper profile for personalized deals and member discounts.")

        with st.form("shopper_signup_form"):
            c_name, c_email = st.columns(2)
            name = c_name.text_input("Full Name", placeholder="Jane Doe")
            email = c_email.text_input("Email Address", placeholder="jane@example.com")
            password = st.text_input("Password", type="password", placeholder="Choose a secure password")

            st.markdown("##### 👤 Member Profile Details")
            d1, d2, d3 = st.columns(3)
            gender = d1.selectbox("Gender", ["M", "F"], format_func=lambda x: "Male" if x == "M" else "Female")
            age = d2.selectbox("Age Bracket", ["0-17", "18-25", "26-35", "36-45", "46-50", "51-55", "55+"], index=2)
            city_cat = d3.selectbox("City Category", ["A", "B", "C"], format_func=lambda x: f"Metro Tier {x}")

            d4, d5, d6 = st.columns(3)
            marital = d4.selectbox("Marital Status", [0, 1], format_func=lambda x: "Single" if x == 0 else "Married")
            occ_id = d5.selectbox("Occupation", list(OCCUPATION_MAP.keys()), format_func=lambda x: OCCUPATION_MAP[x])
            stay_years = d6.selectbox("Years in Current City", ["0", "1", "2", "3", "4+"], index=2)

            signup_submitted = st.form_submit_button("Register & Activate Profile", width="stretch")

            if signup_submitted:
                if not name or not email or not password:
                    st.error("Please fill in all required fields (Name, Email, Password).")
                else:
                    payload = {
                        "name": name.strip(),
                        "email": email.strip().lower(),
                        "password": password,
                        "gender": gender,
                        "age": age,
                        "city_category": city_cat,
                        "marital_status": marital,
                        "occupation": occ_id,
                        "stay_in_current_city_years": stay_years,
                    }
                    with st.spinner("Creating your member profile..."):
                        resp = client.signup(payload)

                    if resp and "_error" not in resp and "access_token" in resp:
                        token = resp["access_token"]
                        st.session_state["shopper_token"] = token
                        client.set_token(token)
                        st.session_state["shopper_user"] = {
                            "user_id": resp.get("user_id"),
                            "name": resp.get("name"),
                        }
                        st.success("Account created successfully! Welcome to the store.")
                        st.rerun()
                    else:
                        err_msg = resp.get("_error") if isinstance(resp, dict) else "Registration failed."
                        st.error(f"Sign-up error: {err_msg}")


def render_shopper_experience(client: APIClient):
    """Renders the logged-in shopper portal, personalized recommendations, and instant price quote."""
    token = st.session_state.get("shopper_token")
    if not token:
        _render_auth_screen(client)
        return

    # Ensure token is set on client
    client.set_token(token)

    # Fetch fresh user profile
    me = client.get_me()
    if not me:
        st.warning("Session expired. Please log in again.")
        st.session_state["shopper_token"] = None
        client.clear_token()
        st.rerun()
        return

    # ---------------- 1. USER PROFILE HEADER ----------------
    col_u1, col_u2 = st.columns([4, 1])
    with col_u1:
        st.title(f"👋 Welcome back, {me.get('name')}!")
        st.markdown(
            "<span style='background:#ebf8fa; color:#234e52; padding:5px 12px; border-radius:8px; font-weight:600;'>🌟 Preferred Member</span>",
            unsafe_allow_html=True
        )
    with col_u2:
        if st.button("🚪 Log Out", width="stretch"):
            st.session_state["shopper_token"] = None
            client.clear_token()
            st.rerun()

    # User Profile Details Card
    with st.expander("👤 Member Profile & Preferences", expanded=False):
        p1, p2, p3, p4 = st.columns(4)
        p1.metric("Member ID", f"MEMBER-{me.get('user_id'):05d}")
        gender_label = "Male" if me.get("gender") == "M" else "Female"
        p2.metric("Profile", f"{gender_label} • Age {me.get('age')}")
        p3.metric("Location Tier", f"Tier {me.get('city_category')}")
        occ_desc = OCCUPATION_MAP.get(me.get("occupation", 0), "General")
        p4.metric("Occupation", occ_desc)

    st.markdown("---")

    # ---------------- 2. PRODUCT CATALOG & RECOMMENDATIONS ----------------
    st.subheader("🛒 Product Catalog & Recommendations")
    st.caption("Explore trending products and personalized pairings.")

    catalog_data = client.get_catalog(limit=50) or FALLBACK_CATALOG
    catalog_pids = [p["product_id"] for p in catalog_data]

    # Pre-select product if user clicked from recommendation
    default_idx = 0
    if "selected_product_id" in st.session_state and st.session_state["selected_product_id"] in catalog_pids:
        default_idx = catalog_pids.index(st.session_state["selected_product_id"])

    c_select, c_quick = st.columns([2, 3])
    with c_select:
        selected_pid = st.selectbox("Select Product to Inspect", catalog_pids, index=default_idx)
        st.session_state["selected_product_id"] = selected_pid
    
    with c_quick:
        match_cat = next((p for p in catalog_data if p["product_id"] == selected_pid), None)
        if match_cat:
            q1, q2 = st.columns(2)
            q1.metric("Orders Placed", f"{match_cat.get('order_count', 0):,}")
            q2.metric("Popularity Status", "🔥 Top Seller")

    # Fetch product recommendations
    with st.spinner("Loading recommendations..."):
        product_detail = client.browse_product(selected_pid)

    if product_detail:
        b1, b2 = st.columns(2)
        with b1:
            st.markdown("##### 📦 Frequently Bought Together")
            bundles = product_detail.get("apriori_bundles") or []
            if bundles:
                st.write("Customers who bought this item also purchased:")
                cols = st.columns(min(len(bundles[:4]), 4))
                for i, b in enumerate(bundles[:4]):
                    with cols[i]:
                        if st.button(f"🔍 {b}", key=f"btn_bundle_{b}_{i}", width="stretch"):
                            st.session_state["selected_product_id"] = b
                            st.rerun()
            else:
                st.write("*Discover more products in the catalog.*")

        with b2:
            st.markdown("##### ✨ Customers Also Liked")
            similars = product_detail.get("item2vec_similar") or []
            if similars:
                st.write("Similar popular items you might enjoy:")
                cols_sim = st.columns(min(len(similars[:4]), 4))
                for j, s in enumerate(similars[:4]):
                    with cols_sim[j]:
                        if st.button(f"🛍️ {s}", key=f"btn_sim_{s}_{j}", width="stretch"):
                            st.session_state["selected_product_id"] = s
                            st.rerun()
            else:
                st.write("*Discover more products in the catalog.*")

    st.markdown("---")

    # ---------------- 3. PRICE ESTIMATOR & INSTANT CHECKOUT ----------------
    st.subheader("🏷️ Instant Personalized Price & Checkout")
    st.markdown("Get your personalized member price quote and complete your order.")

    col_e1, col_e2 = st.columns(2)
    with col_e1:
        st.markdown(f"**Selected Product:** `{selected_pid}`")
        cat1 = st.number_input("Primary Category", min_value=1, max_value=20, value=3, step=1)
        use_auto_impute = st.checkbox("✨ Auto-detect product specifications", value=True)

        if not use_auto_impute:
            cat2 = st.number_input("Secondary Category", min_value=1, max_value=20, value=5, step=1)
            cat3 = st.number_input("Sub-Category", min_value=1, max_value=20, value=14, step=1)
        else:
            cat2 = None
            cat3 = None
            st.caption("✨ Smart category specifications automatically resolved.")

        btn_predict = st.button("⚡ Calculate My Price", width="stretch")

    with col_e2:
        if btn_predict or st.session_state.get(f"last_pred_{selected_pid}"):
            payload = {
                "product_id": selected_pid,
                "product_category_1": int(cat1),
                "product_category_2": int(cat2) if cat2 else None,
                "product_category_3": int(cat3) if cat3 else None,
            }
            with st.spinner("Calculating personalized quote..."):
                pred_resp = client.predict_shopper_price(payload)

            if pred_resp and "_error" not in pred_resp:
                st.session_state[f"last_pred_{selected_pid}"] = pred_resp
                pred_usd = pred_resp.get("predicted_usd", 0.0)

                st.markdown("##### 💵 Your Tailored Member Price")
                st.metric(
                    label="Personalized Price",
                    value=f"${pred_usd:,.2f}",
                )
                st.caption("Special Holiday Member Deal • Guaranteed Best Price")

                # Purchase button
                if st.button(f"🛒 Confirm Purchase for ${pred_usd:,.2f}", type="primary", width="stretch"):
                    with st.spinner("Processing purchase..."):
                        purch_resp = client.purchase_product(payload)
                    if purch_resp and "_error" not in purch_resp:
                        st.success(f"🎉 Order placed successfully! Order #{purch_resp.get('id')}.")
                        st.rerun()
                    else:
                        err = purch_resp.get("_error") if isinstance(purch_resp, dict) else "Purchase failed."
                        st.error(f"Order could not be processed: {err}")
            else:
                err = pred_resp.get("_error") if isinstance(pred_resp, dict) else "Price calculation failed."
                st.error(f"Quote calculation error: {err}")
        else:
            st.info("Click **Calculate My Price** to see your tailored price for this item.")

    st.markdown("---")

    # ---------------- 4. PURCHASE HISTORY ----------------
    st.subheader("📜 Your Purchase History")
    history_resp = client.get_purchase_history()

    if history_resp and "purchases" in history_resp and history_resp["purchases"]:
        purchases = history_resp["purchases"]
        hdf = pd.DataFrame(purchases)
        
        # Summary row
        h1, h2, h3 = st.columns(3)
        h1.metric("Total Purchases", f"{len(hdf)}")
        h2.metric("Total Spent", f"${hdf['predicted_usd'].sum():,.2f}")
        h3.metric("Average Order", f"${hdf['predicted_usd'].mean():,.2f}")

        # Display table
        display_cols = ["id", "product_id", "product_category_1", "predicted_usd", "purchased_at"]
        clean_cols = [c for c in display_cols if c in hdf.columns]
        st.dataframe(
            hdf[clean_cols].rename(columns={
                "id": "Order #",
                "product_id": "Product ID",
                "product_category_1": "Category",
                "predicted_usd": "Price Paid ($)",
                "purchased_at": "Order Date"
            }),
            width="stretch",
            hide_index=True
        )
    else:
        st.write("*No orders yet. Calculate your price above to place your first order!*")
