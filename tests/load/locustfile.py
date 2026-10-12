import random

from locust import HttpUser, between, task

try:  # locust runs this file as a script from its own directory
    from locust_auth import LOCUST_PASSWORD, acquire_token
except ImportError:  # imported as part of the tests package
    from tests.load.locust_auth import LOCUST_PASSWORD, acquire_token


class BlackFridayAPIUser(HttpUser):
    wait_time = between(0.01, 0.05)  # High-frequency load testing

    def on_start(self):
        """Authenticates user on startup and sets Bearer Token header."""
        email = f"locust_user_{random.randint(1, 100000)}@example.com"
        signup_payload = {
            "name": "Locust Tester",
            "email": email,
            "password": LOCUST_PASSWORD,
            "gender": random.choice(["M", "F"]),
            "age": random.choice(["18-25", "26-35", "36-45", "46-50", "51-55"]),
            "city_category": random.choice(["A", "B", "C"]),
            "marital_status": random.randint(0, 1),
            "occupation": random.randint(0, 20),
            "stay_in_current_city_years": "2"
        }
        token = acquire_token(self.client, signup_payload)
        if token:
            self.client.headers["Authorization"] = f"Bearer {token}"

    @task(3)
    def health_check(self):
        self.client.get("/health")

    @task(5)
    def analytics_summary(self):
        self.client.get("/analytics/summary")

    @task(4)
    def analytics_demographics(self):
        dim = random.choice(["gender", "age", "city_category", "marital_status", "occupation"])
        self.client.get(f"/analytics/demographics/{dim}")

    @task(6)
    def shopper_curated_catalog(self):
        self.client.get("/shopper/curated-catalog")

    @task(4)
    def shopper_catalog_browse(self):
        self.client.get("/shopper/catalog?limit=20")

    @task(10)
    def ml_predict_price_authenticated(self):
        """Authenticated ML Endpoint: Single item ONNX price prediction."""
        payload = {
            "product_category_1": random.randint(1, 18),
            "product_category_2": random.randint(1, 18),
            "product_category_3": random.randint(1, 18),
            "product_id": f"P000{random.randint(10000, 99999)}"
        }
        self.client.post("/shopper/predict-price", json=payload)

    @task(8)
    def ml_predict_price_batch(self):
        """ML Endpoint: Batch item ONNX price prediction."""
        items = []
        for _ in range(5):
            items.append({
                "product_category_1": random.randint(1, 18),
                "product_category_2": random.randint(1, 18),
                "product_category_3": random.randint(1, 18),
                "product_id": f"P000{random.randint(10000, 99999)}"
            })
        self.client.post("/shopper/predict-price-batch", json={"items": items})

    # =========================================================================
    # Phase 4 Conversational AI & Caching Tasks
    # =========================================================================
    @task(7)
    def bot_stream_conversation(self):
        """Phase 4: Bot SSE Streaming Endpoint (/bot/stream)."""
        queries = [
            "Find silk shirts under $80 in size L",
            "What materials is P00025442 made of and how do I wash it?",
            "What is your holiday return policy?",
            "Show me top deals on jackets and winter wear",
            "Recommend complete stylish bundle with accessories",
        ]
        q = random.choice(queries)
        self.client.post(
            "/bot/stream",
            json={"query": q, "user_id": "locust_user", "mode": "text"},
            headers={"Accept": "text/event-stream"},
        )

    @task(5)
    def bot_stream_tier0_cache_hit(self):
        """Phase 4: Sub-millisecond Tier-0 6h exact cache hit."""
        fixed_query = "Find silk shirts under $80 in size L"
        self.client.post(
            "/bot/stream",
            json={"query": fixed_query, "user_id": "locust_cached_user", "mode": "text"},
            headers={"Accept": "text/event-stream"},
        )

