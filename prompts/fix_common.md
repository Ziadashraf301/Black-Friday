# Common rules for ALL fix sessions (read this first, every time)

You are a fixer agent for the "Black Friday v2" repository. Your work order is one work package (WP) file in prompts/. Fix IDs refer to FIX_PLAN.md; evidence for each finding is in REVIEW_*.md. If RESTRUCTURE_MAP.md exists, files may have MOVED since FIX_PLAN.md was written: use the map to find the current path.

## 1. Safety and git
- Run `git branch --show-current`. It must be `review-fixes`. If not, STOP and tell me.
- Never open or print .env. Never touch legacy_project/. Do not delete or overwrite models/ artifacts or data/ files unless your WP says so.
- Commit only on this branch, only when your tests are green. Stage ONLY the files you changed with `git add <paths>` (never `git add -A`, never `git add .`). Confirm .env is not staged with `git status`. Never push, never `git reset --hard`, never force anything, never switch branches.
- If you cannot get tests green, do NOT commit. Undo only your own edits with `git checkout -- <your files>` and explain in your report.

## 2. Verify before you edit
- For every fix: open the actual file and confirm the finding is still true. Line numbers are hints only. If a finding is wrong, already fixed, or the file no longer exists, mark it SKIPPED with the reason. Do not "fix" things that are not broken.
- Where the fix says "verify metric direction / parameter binding / callers", do that check first and record the evidence.

## 3. You run the tests yourself
- Services: tests may need Postgres/Redis. Run `docker compose ps`; if they are not up, run `docker compose up -d postgres redis` (and minio/mlflow only if needed).
- Before editing: run the tests related to your files. Compare with baseline_tests.txt (the known-failing set before any fix). Do not blame yourself for pre-existing failures, but list them.
- After each fix group: run the affected tests. At the end: run the full suite `pytest tests/ -q`. Fix every failure you caused.
- Never delete, skip, or weaken a test to make it pass. A test may be changed only if it asserted the buggy behaviour or an old path; explain each such change in your report.
- Add a small regression test for every bug fix (in tests/, name it after the fix id, e.g. test_fix_2_4_champion_gate.py). The test must fail on the old code and pass on the new code.
- "Looks right" is not verification. For each fix record HOW you proved it (test name, command, output).

## 4. Structure: you may reorganize, with rules
You may move, rename, or split files inside your domain when it removes duplication or fixes layering. When you do:
- Use `git mv`. Update ALL references across the whole repo: imports, tests, Dockerfiles, docker-compose.yml, Makefile, CI workflow, README/docs mentions.
- Append every move to RESTRUCTURE_MAP.md as `old path -> new path (reason)`.
- Do not leave compatibility shims unless a test needs one; if you do, mark them TODO-remove in the report.

Target layout and dependency rule:
```
core/        config, logging, security, exceptions, cache/, db/, tracking/ (shared MLflow client+config+tracing helpers), embeddings/ (shared)
ml/          features, models, market_basket, segmentation, pipelines, tracking/ (ML-specific: model_card, explainability, drift)
ai/          shopping assistant: classifier, extractor, guardrails, nodes, router, services, tools, workflow, observability
evaluation/  separate module: evaluation/ml/ (model + imputation evaluation), evaluation/ai/ (router benchmarks, golden-set runners)
apps/api, apps/reflex_app
```
Allowed imports: core imports NOTHING from ml/ai/apps/evaluation. ml and ai import core only (never apps, never each other's pipelines). apps may import core, ai, ml. evaluation may import anything, and NOTHING imports evaluation. MLflow is configured and its client created ONLY in core/tracking; ml and ai use it through core.tracking helpers (no direct client setup elsewhere). tests/test_architecture.py (created in WP0) enforces this: keep it green.

## 5. Scope
Do only your WP's fixes. If you notice other problems, list them under "Noticed but not fixed". Do not start other WPs.

## 6. Decisions already made (do not ask)
- 2.3 passwords: pre-hash scheme with UPGRADE-ON-LOGIN: verify new scheme first, fall back to the legacy hash, re-hash and store on successful legacy login. Existing users must keep working.
- 4.7: create requirements-ui.txt (yes).
- 6.7: voice toggle becomes a disabled "Beta soon" badge.
- 8.1: composition with backward-compatible delegation; callers keep working; tests prove it.
- 9.3: remove the dead get_db() only if nothing imports it. Do NOT build a Unit of Work.
- 10.1: add core/exceptions.py with domain exceptions, map them to HTTP in apps/api/main.py handlers.
- 10.4: docstring/doc fix only. No Celery/ARQ.
- 10.6: resolved by the move to evaluation/ (WP0).

## 7. Report (mandatory)
Write FIX_REPORT_<wp>.md in the project root: for each fix id: DONE / SKIPPED / PARTIAL, evidence of verification (test names and results), files changed/moved, decisions, and "Noticed but not fixed". Then commit (code + report) with message `<WP>: <summary>`. Final chat message: at most 15 lines.

## 8. Test hygiene (added after WP2)
Tests that touch Redis must use a dedicated database (REDIS_DB=15, set in tests/conftest.py) and unique key prefixes. Never flush or pattern-delete in DB 0. If your WP touches code that a baseline-failing test exercises, fix that test too.
Repositories (core/db/repositories) are pure data access: no cache logic. Caching policy lives in services, using core/cache/decorators.py.
