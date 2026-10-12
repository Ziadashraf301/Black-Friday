import socket
from pathlib import Path
from urllib.parse import urlparse

import pytest

from core.config import settings
from tests.helpers import mark_folder

LOOPBACK = {"127.0.0.1", "::1", "localhost"}
# Ports of the project's services; asyncio's socketpair() on Windows connects to random loopback ports, which stays allowed
SERVICE_PORTS = {
    settings.POSTGRES_PORT,
    settings.REDIS_PORT,
    settings.MINIO_PORT,
    settings.API_PORT,
    settings.REFLEX_PORT,
    urlparse(settings.MLFLOW_TRACKING_URI).port,
    5432,
    6379,
    5000,
    9000,
    9001,
    8000,
    8001,
    3000,
}
_real_connect = socket.socket.connect
_real_connect_ex = socket.socket.connect_ex


def _is_blocked(address) -> bool:
    if not isinstance(address, tuple) or len(address) < 2:
        return False  # AF_UNIX
    host, port = address[0], address[1]
    return host not in LOOPBACK or port in SERVICE_PORTS


def pytest_collection_modifyitems(config, items):
    mark_folder(items, Path(__file__).parent, "unit")


@pytest.fixture(autouse=True)
def no_network(monkeypatch):
    """Unit tests use mocks only: opening a connection to a service, or to any other host, fails immediately.

    A test that needs a live service belongs in tests/integration (or tests/e2e).
    """

    def _refuse(address):
        raise ConnectionRefusedError(f"network access to {address} is blocked in tests/unit; use tests/integration")

    def connect(self, address):
        if _is_blocked(address):
            _refuse(address)
        return _real_connect(self, address)

    def connect_ex(self, address):
        if _is_blocked(address):
            _refuse(address)
        return _real_connect_ex(self, address)

    def refuse_db(*args, **kwargs):
        raise ConnectionRefusedError("PostgreSQL access is blocked in tests/unit; use tests/integration")

    monkeypatch.setattr(socket.socket, "connect", connect)
    monkeypatch.setattr(socket.socket, "connect_ex", connect_ex)
    try:
        import psycopg2

        monkeypatch.setattr(psycopg2, "connect", refuse_db)
    except ImportError:  # pragma: no cover
        pass
