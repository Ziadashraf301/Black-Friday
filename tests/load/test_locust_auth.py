from tests.load.locust_auth import AUTH_MAX_ATTEMPTS, acquire_token

PAYLOAD = {"email": "u@example.com", "password": "pw"}


class _Resp:
    def __init__(self, status, body=None):
        self.status_code = status
        self._body = body or {}

    def json(self):
        return self._body


class _Client:
    def __init__(self, script):
        self.script = list(script)
        self.calls = []

    def post(self, url, json=None):
        self.calls.append(url)
        return self.script.pop(0)


def test_signup_success_returns_token():
    client = _Client([_Resp(201, {"access_token": "T"})])
    assert acquire_token(client, PAYLOAD, sleep=lambda s: None) == "T"
    assert client.calls == ["/auth/signup"]


def test_rate_limited_signup_is_retried_after_sleeping():
    sleeps = []
    client = _Client([_Resp(429), _Resp(429), _Resp(201, {"access_token": "T"})])
    assert acquire_token(client, PAYLOAD, sleep=sleeps.append) == "T"
    assert len(sleeps) == 2


def test_existing_account_falls_back_to_login():
    client = _Client([_Resp(409), _Resp(200, {"access_token": "L"})])
    assert acquire_token(client, PAYLOAD, sleep=lambda s: None) == "L"
    assert client.calls == ["/auth/signup", "/auth/login"]


def test_rate_limited_login_is_retried():
    client = _Client([_Resp(409), _Resp(429), _Resp(409), _Resp(200, {"access_token": "L"})])
    assert acquire_token(client, PAYLOAD, sleep=lambda s: None) == "L"


def test_gives_up_after_max_attempts():
    client = _Client([_Resp(429)] * AUTH_MAX_ATTEMPTS)
    assert acquire_token(client, PAYLOAD, sleep=lambda s: None) is None


def test_hard_failure_returns_none_without_retrying():
    client = _Client([_Resp(500), _Resp(401)])
    assert acquire_token(client, PAYLOAD, sleep=lambda s: None) is None
    assert client.calls == ["/auth/signup", "/auth/login"]
