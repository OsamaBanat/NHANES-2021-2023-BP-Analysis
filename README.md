# Demographic, Lifestyle, Metabolic, and Renal Factors Associated with Blood Pressure in U.S. Adults: A Survey-Weighted Analysis and Predictive Modeling Study

## Overview

This project examines the demographic, socioeconomic, anthropometric, metabolic, and renal characteristics associated with systolic and
diastolic blood pressure among US adults. It also evaluates the ability of statistical and regularized regression models to predict blood
pressure using routinely available participant characteristics.

The analysis uses data from the National Health and Nutrition Examination Survey (NHANES) 2021–2023 and combines survey-weighted statistical
inference with a separate machine-learning-style prediction workflow.

## Research Objectives

1. Estimate the adjusted associations between selected demographic, socioeconomic, and clinical characteristics and systolic and diastolic blood pressure.
2. Identify characteristics associated with differences in blood pressure after accounting for other measured covariates.
3. Develop and compare predictive models for systolic and diastolic blood pressure.
4. Evaluate predictive performance across participant subgroups and examine patterns in model residuals.

## Data Source

The study uses publicly available NHANES 2021–2023 data files, including information on:

- Demographics and socioeconomic characteristics.
- Anthropometric measurements.
- Blood pressure measurements and hypertension-related information.
- Biochemical and metabolic measurements.
- Diabetes-related variables.
- Urinary albumin and creatinine measurements.

NHANES data are available from the [CDC National Health and Nutrition Examination Survey](https://www.cdc.gov/nchs/nhanes/).

The analytical dataset includes variables such as age, sex, race/ethnicity, marital status, education, income-to-poverty ratio (IPR), 
insurance status, body mass index (BMI), waist circumference (WC), albumin-to-creatinine ratio (ACR), estimated glomerular filtration 
rate (eGFR), diabetes status, HbA1c, and pulse rate.

Study population: US adults aged 18 years and older.  

## Methodology

### 1. Data Preparation

NHANES data files were imported, merged, and cleaned. The analysis included variable recoding, calculation of derived variables,
assessment of missing data, and examination of distributions and potential outliers.

The primary outcomes were:

- **Systolic blood pressure (SBP):** Average systolic blood pressure.
- **Diastolic blood pressure (DBP):** Average diastolic blood pressure.

ACR and eGFR were log-transformed for analysis.

### 2. Descriptive and Association Analyses

Survey-weighted descriptive statistics were used to characterize the study population. Blood pressure differences across 
categorical characteristics were examined using survey-adjusted statistical tests.

Weighted correlation analyses and multivariable survey-weighted linear regression models were used to assess associations with SBP and DBP.

Sequential regression models were developed to evaluate how associations changed after accounting for additional demographic and
clinical characteristics. Model specifications were assessed based on statistical performance, stability, interpretability, and clinical plausibility.

### 3. Blood Pressure Prediction

A separate predictive modeling workflow was implemented using **tidymodels** in R.

Four regression approaches were evaluated:

- Ordinary least squares (OLS) regression.
- Ridge regression.
- Lasso regression.
- Elastic Net regression.

The dataset was divided into training and testing subsets using an 80:20 split. Model tuning and evaluation used repeated 10-fold cross-validation.

Preprocessing included zero-variance predictor removal, dummy encoding of categorical variables, and normalization of numeric predictors.

Predictive performance was assessed using:

- Root Mean Squared Error (RMSE).
- Mean Absolute Error (MAE).
- Coefficient of determination (R²).

Additional analyses examined subgroup prediction errors, observed-versus-predicted values, and residual patterns.

**Note:** The prediction workflow was conducted separately from the survey-weighted association analysis. Predictive performance should 
therefore not be interpreted as survey-weighted population inference.

## Key Findings

### Adjusted Associations

- Older age and higher log-transformed albumin-to-creatinine ratio were positively associated with systolic blood pressure.
- Higher BMI and pulse rate were positively associated with diastolic blood pressure.
- Female sex was associated with lower SBP and DBP compared with male sex.
- Non-Hispanic Black participants had higher blood pressure than non-Hispanic White participants in the adjusted analyses.
- Some socioeconomic associations observed in simpler models were attenuated after adjustment for clinical characteristics.

### Predictive Performance

The best-performing models achieved approximately:

| Outcome | Test-set RMSE | Test-set MAE | Test-set R² |
| :------ | :------------ | :----------- | :---------- | 
| SBP     | 15.28 mmHg    | 11.62 mmHg   | 0.221       |
| DBP     | 9.67 mmHg     | 7.60 mmHg    | 0.177       |

Regularized regression models provided negligible improvements over ordinary least squares regression.

Subgroup analyses showed variation in prediction errors across demographic and clinical groups. Residual analyses also 
suggested that predictions were less accurate at the extremes of observed blood pressure values.

## Interpretation

The findings demonstrate that characteristics associated with systolic blood pressure are not necessarily identical to those 
associated with diastolic blood pressure.

Although several demographic and clinical characteristics were associated with blood pressure, the predictive models explained 
a relatively modest proportion of its variation. This highlights the distinction between identifying adjusted statistical associations
and accurately predicting individual blood pressure values.

## Limitations

- The cross-sectional study design does not establish temporal relationships or causality.
- The analysis is limited to variables available in the selected NHANES data files.
- Residual confounding and measurement variability may affect the estimated associations.
- Predictive models were evaluated using internal validation and have not undergone external validation.
- Predictive performance was modest, indicating that additional predictors or more flexible modeling approaches may be needed.

## Technologies and Packages

The project was conducted in **R** using packages including:

- `haven`
- `tidyverse`
- `tidymodels`
- `survey`
- `broom`
- `gt`
- `gtsummary`
- `nortest`
- `emmeans`
- `jtools`
- `car`
- `patchwork`

## Reproducibility

To reproduce the analysis:

1. Install R and the required packages.
2. Download the relevant NHANES 2021–2023 data files from the CDC website.
3. Update the data paths or download URLs in the analysis scripts, if necessary.
4. Run the scripts in the documented order.

The random seed used for the prediction train/test split was `1234`.

## Conclusion

This project integrates survey-weighted epidemiological analysis with predictive modeling to investigate blood pressure among US adults.
It demonstrates the use of complex survey methods for population-level association analyses and cross-validation for evaluating predictive 
models, while highlighting the limitations of modest predictive performance and the importance of distinguishing statistical association from prediction.

## References and Data

- [CDC National Health and Nutrition Examination Survey (NHANES)](https://www.cdc.gov/nchs/nhanes/)
- [NHANES 2021–2023 Data and Documentation](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?Cycle=2021-2023)
