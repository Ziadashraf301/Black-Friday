"""Auth bootstrap for the Locust load test (kept free of locust imports so it is unit-testable)."""

import time


LOCUST_PASSWORD = "LocustPassword123"
AUTH_MAX_ATTEMPTS = 20
AUTH_RETRY_SECONDS = 6.0


def acquire_token(client, signup_payload, sleep=time.sleep):
    """
    Signs up (falling back to login) and returns the JWT, or None.
    /auth/* is rate limited per client IP and all simulated users share one IP, so a 429 is retried after a pause
    instead of leaving the user without a token (which would turn every authenticated task into a 401).
    """
    credentials = {"email": signup_payload["email"], "password": signup_payload["password"]}
    for _ in range(AUTH_MAX_ATTEMPTS):
        resp = client.post("/auth/signup", json=signup_payload)
        if resp.status_code != 429:
            if resp.status_code == 201 and "access_token" in resp.json():
                return resp.json()["access_token"]
            # Already registered (or other signup failure): try logging in instead
            login_resp = client.post("/auth/login", json=credentials)
            if login_resp.status_code == 200 and "access_token" in login_resp.json():
                return login_resp.json()["access_token"]
            if login_resp.status_code != 429:
                return None
        sleep(AUTH_RETRY_SECONDS)
    return None
