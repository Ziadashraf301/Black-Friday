import random
from locust import HttpUser, task, between


class BlackFridayAPIUser(HttpUser):
    wait_time = between(0.01, 0.05)  # High-frequency load testing

    def on_start(self):
        """Authenticates user on startup and sets Bearer Token header."""
        email = f"locust_user_{random.randint(1, 100000)}@example.com"
        signup_payload = {
            "name": "Locust Tester",
            "email": email,
            "password": "LocustPassword123",
            "gender": random.choice(["M", "F"]),
            "age": random.choice(["18-25", "26-35", "36-45", "46-50", "51-55"]),
            "city_category": random.choice(["A", "B", "C"]),
            "marital_status": random.randint(0, 1),
            "occupation": random.randint(0, 20),
            "stay_in_current_city_years": "2"
        }
        resp = self.client.post("/auth/signup", json=signup_payload)
        if resp.status_code == 201 and "access_token" in resp.json():
            token = resp.json()["access_token"]
            self.client.headers["Authorization"] = f"Bearer {token}"
        else:
            # Fallback to login if already exists
            login_resp = self.client.post("/auth/login", json={"email": email, "password": "LocustPassword123"})
            if login_resp.status_code == 200 and "access_token" in login_resp.json():
                token = login_resp.json()["access_token"]
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
