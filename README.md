# Mental Health in Tech: Predicting Work Interference

The OSMI 2014 "Mental Health in Tech" survey, 992 respondents after
cleaning, predicting how often mental health interferes with work
(`work_interfere`: Never, Rarely, Sometimes, Often). Source data:
[Kaggle: OSMI Mental Health in Tech Survey](https://www.kaggle.com/osmi/mental-health-in-tech-survey).
Original files (4 separate R Markdown notebooks: cleaning/EDA, a
productivity-loss calculation, and RF/GBM/XGBoost plus H2O AutoML
modeling) are kept in `_old/` untouched. This rebuild is Python/Jupyter,
same overall structure, ported to pandas/scikit-learn.

The original notebook's last line across its modeling files is "all the
models are nearly 50% accurate", with no baseline computed to check
whether that's actually good. One of the modeling notebooks also
configures XGBoost with `num_class = 20` despite `work_interfere` only
having 4 levels.

## Notebooks

1. `00_data_setup_eda.ipynb`: same cleaning decisions as the original
   (drop comments/state, fill self_employed, consolidate ~49 gender
   spellings into Male/Female/Trans), a more defensible fix for invalid
   ages (median imputation on a plausible range instead of arbitrary
   replacement values).
2. `01_statistical_testing.ipynb`: chi-square tests. Two of the
   original's "the chart shows a relationship" claims (remote_work,
   tech_company) don't hold up formally; family_history and Gender do.
3. `02_feature_engineering_selection.ipynb`: one-hot encoding, and a
   caught-and-fixed mutual information bug, a sparse 7-respondent column
   dominated the naive ranking until the estimator was told its inputs
   are discrete, not continuous.
4. `03_model_training_evaluation.ipynb`: random forest, gradient
   boosting, and XGBoost (with `num_class` fixed to 4) against an actual
   majority-class baseline, something the original never computed.
5. `04_clustering.ipynb`: KMeans and Gaussian mixture clustering by
   workplace mental-health culture alone, checked against real
   treatment-seeking and work-interference rates afterward.

## Results

Gradient boosting reaches 47.7% accuracy against a 46.7% majority-class
baseline, barely better. Random forest (36.2%) and XGBoost (42.7%) both
fall below the baseline on raw accuracy, though random forest's macro-F1
(0.325 versus 0.000 for the baseline) shows real signal spread across
all 4 classes that raw accuracy hides. Family history of mental illness
remains the single strongest, most consistent predictor across every
method used here.

Full write-up with charts: `docs/index.html` (also published via GitHub
Pages).

## Future work

- Model `treatment` (binary, better balanced) as a second target for
  direct comparison against the harder 4-class `work_interfere` result.
- Try an ordinal classification approach instead of treating the 4
  work-interference levels as unordered classes.
- Revisit the original's productivity-loss calculation and H2O AutoML
  notebooks, not reproduced here, as their own dedicated follow-ups.
