"""
Server-verified caller identity shared by the rate limiter, ban middleware and bot routes.
Only a valid Bearer JWT `sub` or the client address (socket peer, or X-Forwarded-For via a
configured trusted proxy) is trusted; caller-supplied
X-User-ID headers and user_id query/body parameters are never used.
"""

import hashlib
import hmac
import ipaddress
import re
from functools import lru_cache
from typing import Any, Optional, Tuple, Union

from fastapi import Request
from starlette.requests import HTTPConnection

from core.config import settings
from core.logging import get_logger
from core.security import decode_access_token

logger = get_logger(__name__)

IPAddress = Union[ipaddress.IPv4Address, ipaddress.IPv6Address]
IPNetwork = Union[ipaddress.IPv4Network, ipaddress.IPv6Network]


def verified_user_id(request: Request) -> Optional[str]:
    """Returns the `sub` of a valid Bearer JWT, or None when absent/invalid/expired."""
    auth_header = request.headers.get("Authorization")
    if not auth_header or not auth_header.startswith("Bearer "):
        return None
    try:
        sub = decode_access_token(auth_header.split(" ", 1)[1].strip()).get("sub")
    except Exception:
        return None
    return str(sub) if sub else None


DEFAULT_SESSION_ID = "default_session"
_SESSION_ID_RE = re.compile(r"^[A-Za-z0-9_.:-]{1,64}$")


@lru_cache(maxsize=8)
def _parse_trusted_proxies(raw: Optional[str]) -> Tuple[IPNetwork, ...]:
    networks = []
    for item in (raw or "").split(","):
        item = item.strip()
        if not item:
            continue
        try:
            networks.append(ipaddress.ip_network(item, strict=False))
        except ValueError:
            logger.warning(f"Ignoring invalid TRUSTED_PROXIES entry: {item!r}")
    return tuple(networks)


def _parse_ip(value: str) -> Optional[IPAddress]:
    try:
        return ipaddress.ip_address(value.strip())
    except ValueError:
        return None


def _is_trusted(ip: Optional[IPAddress], trusted: Tuple[IPNetwork, ...]) -> bool:
    return ip is not None and any(ip in net for net in trusted)


def client_ip(conn: HTTPConnection) -> str:
    """
    Client address for rate limits, bans and guest ids (HTTP requests and WebSockets).
    X-Forwarded-For is honoured only when the socket peer is in TRUSTED_PROXIES; the list is then walked
    from the right and the first hop that is not a trusted proxy is returned, so a client cannot spoof
    its address by prepending entries. With TRUSTED_PROXIES empty (the default) this is the socket peer.
    """
    peer = conn.client.host if conn.client else "unknown_ip"
    trusted = _parse_trusted_proxies(settings.TRUSTED_PROXIES)
    if not trusted or not _is_trusted(_parse_ip(peer), trusted):
        return peer
    hops = [h for value in conn.headers.getlist("x-forwarded-for") for h in value.split(",")]
    result = peer
    for hop in reversed(hops):
        hop_ip = _parse_ip(hop)
        if hop_ip is None:
            # Malformed entry: stop at the last address a trusted proxy vouched for.
            return result
        result = str(hop_ip)
        if not _is_trusted(hop_ip, trusted):
            return result
    return result


def sanitize_session_id(session_id: Any) -> str:
    """Returns the client session id when it is a short safe token, otherwise the default."""
    if isinstance(session_id, str) and _SESSION_ID_RE.match(session_id):
        return session_id
    return DEFAULT_SESSION_ID


def guest_user_id(ip: str, session_id: Any) -> str:
    """
    Per-visitor guest id derived from the peer address and session id with an HMAC keyed by SECRET_KEY.
    Strangers on different addresses never share cart/profile state, ids cannot be guessed or forged
    by a client, and the `guest_` prefix can never collide with a numeric shopper id.
    """
    msg = f"{ip}|{sanitize_session_id(session_id)}".encode("utf-8")
    digest = hmac.new(settings.SECRET_KEY.encode("utf-8"), msg, hashlib.sha256).hexdigest()
    return f"guest_{digest[:24]}"


def bot_user_id(request: Request, session_id: Any = None) -> str:
    """Shopper id for the assistant: verified JWT subject, otherwise a per-session guest id."""
    return verified_user_id(request) or guest_user_id(client_ip(request), session_id)


def resolve_client_identity(request: Request) -> str:
    """Verified JWT subject when present, otherwise the client IP."""
    return verified_user_id(request) or client_ip(request)
