"""Identity constants shared by the graph nodes and schemas."""

# Fallback owner for graph callers that supply no user_id. Everything keyed on it (carts, response caches,
# strike counters) is shared between those callers; the HTTP API never relies on it because it always passes
# a verified shopper id or a per-visitor guest id (see apps/api/client_identity.py).
GUEST_USER_ID = "guest_user"
