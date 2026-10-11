# Code Review Runbook — Round 1

> Instructions for the AI agent. The user starts each session with:
> **"Read docs/REVIEW_PLAN.md and do the next step."**

## Rules (every session)

1. Read this file and `docs/REVIEW_LOG.md` first. The next step is the first one below not marked `[x]`.
2. Do **one step per session**, then stop and tell the user to run `/clear`.
3. Work on branch `review/round-1`. Never push. Commit only after the user approves the diff.
4. Review level: `medium` (use `low` only where noted). Review only the listed paths.
5. Don't re-report findings already in the log as fixed / deferred / won't fix.
6. Fix only `high`/`medium` severity bugs unless the user says otherwise; log the rest as `deferred`.
7. Every fix gets a test where practical. After fixing, run only the tests related to the touched files (plus `tests/test_architecture.py`), one file at a time with `--timeout=45`; the full suite hangs on Windows (see the Baseline in the log). Lint only the touched files (flake8, black --check, isort --check). If a test takes a long time, stop and find out why.
8. Follow all CLAUDE.md rules (layering, no `print()`, config via `settings`, mlflow only in `core/tracking/`).

## Per-step procedure

1. Review the step's paths (use the `code-review` skill at the given level, or `security-review` for step 1).
2. Add findings to `docs/REVIEW_LOG.md` with IDs (`SEC-1`, `API-1`, `CORE-1`, `SRV-1`, `PIPE-1`, `ANL-1`, `AI-1`, `UI-1`, `INF-1`), file:line, severity, status `open`.
3. Show the user a short table and ask which to fix.
4. Fix the approved findings, add tests, run tests and lint.
5. Show the diff summary. After the user approves, commit as `review(<area>): <summary> (<IDs>)`.
6. Update the log statuses with the commit hash, mark the step `[x]` below, and tell the user to `/clear`.

## Steps

- [x] **0. Baseline:** run `make test` and `make lint`. Record the pre-existing failures in the log's Baseline section. Don't fix anything.
- [x] **1. Security:** `security-review` on auth, JWT, bcrypt, rate limiting, `SecurityBanMiddleware`, CORS (`apps/api/auth.py`, `apps/api/middleware/`, `apps/api/rate_limiting/`, `core/security/`, `apps/api/main.py`).
- [x] **2. API:** `apps/api` (routes, services, schemas, dependencies, handlers).
- [ ] **3. Core:** `core` (config, db + repositories, cache, tracking, embeddings).
- [ ] **4. ML serving:** `ml/serving`, `ml/models` (ONNX loading, input schema mismatches, metrics).
- [ ] **5. ML pipelines:** `ml/pipelines`, `ml/features` (data leakage, champion/challenger promotion, drift thresholds, cron cache flush).
- [ ] **6. ML analytics:** `ml/segmentation`, `ml/market_basket`.
- [ ] **7. AI assistant:** `ai` (guardrails, prompt injection, LangGraph workflow, tool errors).
- [ ] **8. Frontend:** `apps/reflex_app`, level `low`.
- [ ] **9. Infra:** `docker-compose.yml`, `docker/`, `.github/workflows/`, `Makefile`, `requirements.txt` (misconfigurations, secrets, unpinned versions, CI gaps).
- [ ] **10. Wrap-up:** rerun tests and lint, compare with the baseline, write a summary at the top of the log, and suggest round-2 items from the `deferred` findings.
