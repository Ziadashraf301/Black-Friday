# WP4: ML quick bug fixes (no retraining)

Read prompts/fix_common.md and follow it. Fixes from FIX_PLAN.md: 2.4, 2.5, 3.4, 3.5, 5.5, 8.6, 10.3, 10.4, and the DEDUPLICATION half of 4.2 (extract the duplicated ColumnTransformer into one factory function in ml/models/regression.py with NO behaviour change; the occupation reclassification is NOT part of this WP, it is in WP11).

Required checks and tests:
- 2.4 ml/pipelines/retrain.py: BEFORE changing, read the code and the metric: confirm it is R2 (higher is better) and what min_improvement_delta means. The promotion condition must be: candidate_metric >= champion_metric + min_improvement_delta. Write a table-driven test with synthetic metrics (candidate worse, equal, better by less than delta, better by more than delta) covering the promote/no-promote decision. If the real metric is an error (lower is better), say so and adapt the direction accordingly.
- 3.5 train.py: --model aliases resolve through ModelRegistry.resolve_name (lgbm -> lightgbm etc). Test every alias and an unknown name (clean error). Also run `python -m ml.pipelines.train --help`.
- 5.5 monitor.py: ground-truth check must look at the evaluation dataframe (curr_eval) or the raw batch column. Test: a batch containing "purchase" sets has_target True; a batch without it sets False.
- 3.4 data_contract.py: add stay_in_current_city_years validation ("0","1","2","3","4+"). First check what values the real data/ingest produce so you do not reject valid data; test valid and invalid values.
- 2.5 export ModelRegistry from ml/models/__init__.py; test the import.
- 8.6 tests/test_features.py: assert pandera SchemaError, not bare Exception.
- 10.3 model_card.py: document all 10 features (derive the list from the code, not from the plan); test the generated markdown.
- 10.4 cron_scheduler.py: docstring/doc fix only.
- Verify WP0 results here: no ml/ file imports apps or ai (tests/test_architecture.py), MLflow only via core.tracking.
- Finally run the ml-related tests and `python -m ml.pipelines.monitor --help`, `... retrain --help`.

## Extra: evaluation layering
train.py, retrain.py and preprocess.py import ModelEvaluator from evaluation/ml and are allow-listed in tests/test_architecture.py. Move the metric computation into ml/models/metrics.py (training needs it); evaluation/ml/evaluate.py imports it for reporting and CLI. Remove the three allow-list entries. Results must be identical before and after (test with the same inputs).
