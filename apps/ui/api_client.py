import requests
import os
from typing import Dict, Any, List, Optional
from core.logging import get_logger

logger = get_logger(__name__)

try:
    import streamlit as st
    HAS_STREAMLIT = True
except ImportError:
    HAS_STREAMLIT = False


class APIClient:
    """Dedicated API Client SDK for communicating with the FastAPI Backend Service."""

    def __init__(self, base_url: Optional[str] = None):
        self.base_url = (base_url or os.getenv("API_BASE_URL", "http://localhost:8000")).rstrip("/")
        self.token: Optional[str] = None

    def set_token(self, token: Optional[str]):
        """Sets the JWT Bearer token for authenticated requests."""
        self.token = token

    def clear_token(self):
        """Clears the authentication token."""
        self.token = None

    def _headers(self) -> Dict[str, str]:
        headers = {"Content-Type": "application/json"}
        if self.token:
            headers["Authorization"] = f"Bearer {self.token}"
        return headers

    def _get(self, endpoint: str, params: Optional[Dict[str, Any]] = None) -> Optional[Any]:
        """Private helper for GET requests."""
        url = f"{self.base_url}{endpoint}"
        try:
            resp = requests.get(url, params=params, headers=self._headers(), timeout=10)
            if resp.status_code == 200:
                return resp.json()
            logger.warning(f"GET {endpoint} returned status code {resp.status_code}: {resp.text}")
            return None
        except Exception as e:
            logger.warning(f"API connection error for GET {endpoint}: {e}")
            return None

    def _post(self, endpoint: str, payload: Dict[str, Any]) -> Optional[Any]:
        """Private helper for POST requests."""
        url = f"{self.base_url}{endpoint}"
        try:
            resp = requests.post(url, json=payload, headers=self._headers(), timeout=15)
            if resp.status_code in (200, 201):
                return resp.json()
            logger.warning(f"POST {endpoint} returned status code {resp.status_code}: {resp.text}")
            try:
                err_data = resp.json()
                return {"_error": err_data.get("detail", f"Error {resp.status_code}")}
            except Exception:
                return {"_error": f"Error {resp.status_code}: {resp.text}"}
        except Exception as e:
            logger.warning(f"API connection error for POST {endpoint}: {e}")
            return {"_error": str(e)}

    # --- Health Check ---
    def get_health(self) -> Optional[Dict[str, Any]]:
        """Fetches API health status."""
        return self._get("/health")

    # --- Analytics & EDA ---
    def get_analytics_summary(self) -> Optional[Dict[str, Any]]:
        """Fetches total revenue, orders, customers, and AOV metrics."""
        return self._get("/analytics/summary")

    def get_demographics(self, dimension: str = "gender") -> Optional[List[Dict[str, Any]]]:
        """Fetches demographic distribution for gender, age, city_category, etc."""
        return self._get(f"/analytics/demographics/{dimension}")

    def get_eda_with_stats(self, dimension: str) -> Optional[Dict[str, Any]]:
        """Combined demographic breakdown with embedded empirical statistical significance."""
        return self._get(f"/analytics/eda-with-stats/{dimension}")


    # --- Authentication ---
    def signup(self, payload: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Registers a new user and returns JWT token and cluster assignment."""
        return self._post("/auth/signup", payload=payload)

    def login(self, email: str, password: str) -> Optional[Dict[str, Any]]:
        """Authenticates user and returns JWT token."""
        return self._post("/auth/login", payload={"email": email, "password": password})

    def get_me(self) -> Optional[Dict[str, Any]]:
        """Fetches the logged-in user profile, persona, and cluster info."""
        return self._get("/auth/me")

    # --- Shopper Experience ---
    def get_catalog(self, limit: int = 100) -> Optional[List[Dict[str, Any]]]:
        """Fetches products from the network metrics catalog."""
        return self._get("/shopper/catalog", params={"limit": limit})

    def browse_product(self, product_id: str) -> Optional[Dict[str, Any]]:
        """Fetches product details, association rules, and Item2Vec recommendations."""
        return self._get(f"/shopper/browse/{product_id}")

    def predict_shopper_price(self, payload: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Runs the complete serving pipeline (MissForest Imputer + ONNX Predictor) for tailored price."""
        return self._post("/shopper/predict-price", payload=payload)

    def purchase_product(self, payload: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Records product purchase event in PostgreSQL user_purchases table."""
        return self._post("/shopper/purchase", payload=payload)

    def get_purchase_history(self) -> Optional[Dict[str, Any]]:
        """Fetches the authenticated user's purchase history."""
        return self._get("/shopper/history")
