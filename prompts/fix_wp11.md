# WP11: ML retraining project (run LAST, watch it)

Read prompts/fix_common.md and follow it. Fixes from FIX_PLAN.md: 3.3, 3.6, 6.5, then 4.2 as a separate experiment. This changes model behaviour, so it has extra rules.

Preconditions: Postgres is running with the warehouse data loaded; MLflow/MinIO are up if the pipelines need them. If the data or services are missing, STOP at once and tell me exactly what is missing; do not change code in that case.

## Safety
- Back up first: copy models/ to models_backup_<YYYYMMDD>/ (do not commit the backup; make sure it is git-ignored).
- Never leave the repo in a state where code and artifacts disagree. At the end either both are new and consistent, or you revert the code changes of this WP.

## Part A: remove target leakage (3.3, 3.6, 6.5)
1. Record baseline metrics of the CURRENT artifacts (regression R2/RMSE/MAE on the holdout and the imputation error) using evaluation/ml/. Save to a metrics_before.json.
2. 3.3 ml/features/imputation.py: remove "purchase" from FEATURE_COLS; transform() must work when purchase is absent; clip imputed categories to the valid range [1, 20] (verify the valid range from the data first).
3. 6.5 evaluation/ml evaluate (moved in WP0): drop purchase from the masked test set during holdout imputation evaluation.
4. 3.6 apps/api/serving/imputer.py: keep imputed columns when absent from input; remove the dummy purchase handling; integer conversion that cannot fail on NaN.
5. Re-run the pipeline: preprocess -> train all models -> ONNX export (use the Makefile targets: make preprocess, make train, and the export step). Register in MLflow through core.tracking.
6. Parity test: the serving ONNX imputer and the training MissForestImputer produce the same imputed categories on a sample of rows (allow tiny numeric tolerance). Test that /shopper/predict-price works end to end with the new artifacts and a request that omits product_category_2/3.
7. Metrics after: compare with metrics_before.json. Accept if the final regression R2 is not worse than 0.002 below the old one (leakage removal may legitimately change imputation scores). If it is worse than that, do NOT replace the artifacts: restore from the backup, revert the code, and report the numbers.

## Part B: occupation encoding (4.2 second half), separate commit
Only after Part A is committed. Reclassify occupation from numeric passthrough to categorical encoding in ml/models/regression.py. NOTE: OrdinalEncoder on a nominal feature is questionable for linear models; test OneHot vs Ordinal and keep the better one (justify with metrics). Retrain, re-export, compare to the Part A metrics, keep only if not worse (same 0.002 rule). Update the ONNX input contract and the serving code/schema if the input shape changes, and re-run the end-to-end predict test.

Report: FIX_REPORT_wp11.md with before/after metrics tables, the decisions, and the final artifact list.

## Extra: real baseline
Baseline metrics must come from the REAL holdout evaluation with the current artifacts. The python -m evaluation.ml.evaluate entry point added in WP0 printed round demo numbers (rmse 6.6144, mae 6.25, mse 43.75) that look like a toy example. Do not use them. If the entry point only runs a toy, make it load the real holdout and artifacts first.
