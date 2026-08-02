# Mental Health in Tech: Predicting Work Interference

**Live report:** https://nik8x.github.io/Mental-Health-Analysis-Classification/

The OSMI 2014 "Mental Health in Tech" survey, 992 respondents after
cleaning, predicting how often mental health interferes with work
(`work_interfere`: Never, Rarely, Sometimes, Often). Source data:
[Kaggle: OSMI Mental Health in Tech Survey](https://www.kaggle.com/osmi/mental-health-in-tech-survey).
Cleaning, statistical testing, feature engineering, modeling, and
clustering are all done in Python/Jupyter with pandas and scikit-learn.

Modeling includes random forest, gradient boosting, and XGBoost checked
against an actual majority-class baseline, since "the models are nearly
50% accurate" only means something once you know what a naive baseline
scores.

## Notebooks

1. `00_data_setup_eda.ipynb`: cleaning (drop comments/state, fill
   self_employed, consolidate ~49 gender spellings into
   Male/Female/Trans), median imputation on a plausible age range for
   invalid ages.
2. `01_statistical_testing.ipynb`: chi-square tests on the factors that
   look associated with work_interfere in the EDA charts. remote_work
   and tech_company don't hold up formally; family_history and Gender
   do.
3. `02_feature_engineering_selection.ipynb`: one-hot encoding, and a
   mutual information bug caught along the way: a sparse 7-respondent
   column dominated the naive ranking until the estimator was told its
   inputs are discrete, not continuous.
4. `03_model_training_evaluation.ipynb`: random forest, gradient
   boosting, and XGBoost against a majority-class baseline.
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
- A productivity-loss calculation and an H2O AutoML pass are natural
  next notebooks, not covered here.
