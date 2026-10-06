# How to run the fix sessions

Order: WP0 (alone) -> WP1 -> WP2 -> WP3 -> WP4 -> WP5 -> WP6 -> WP7 -> WP8 -> WP9 -> WP10 -> WP11.
Safe to run in parallel (disjoint files): WP4 with WP9 (ml vs frontend components). Everything else: one at a time.

Setup once:
  git checkout -b review-fixes
  docker compose up -d postgres redis
  pytest tests/ -v 2>&1 | Tee-Object baseline_tests.txt

Launch one WP (change the number):
  agy --dangerously-skip-permissions --effort high -i "Read prompts/fix_wp0.md and follow it exactly."

After each WP: read FIX_REPORT_wpN.md, run `git log --oneline -3` and `git status`.
