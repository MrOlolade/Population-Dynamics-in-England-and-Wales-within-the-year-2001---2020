# Population Dynamics in England & Wales Within The Year 2001 - 2020

A data analytics study of demographic change across 331 local authorities in England and Wales, built in R as part of an MSc Data Science project at the University of Gloucestershire.

## Overview

This project integrates three ONS Mid-Year Population Estimates datasets into a structured analytical panel, then applies statistical testing, regression, clustering and classification to understand how and why populations grew differently across regions between 2001 and 2020.

**Headline finding:** The combined population of England and Wales grew from 52.4M to 59.7M (+14.1%) over the period, with international net migration accounting for 56.6% of that growth. England grew almost twice as fast as Wales (14.4% vs 8.9%).

## Data

Three ONS datasets are used:

| Dataset | Description |
|---|---|
| MYEB1 | 68,068 | Population estimates by age & sex (supporting reference) |
| MYEB2 | 60,242 | Full demographic components — births, deaths, migration (main dataset) |
| MYEB3 | 374 | Summary components (validation benchmark against MYEB2) |

MYEB2 was reshaped from wide format (60,242 rows × 225 columns) into a long panel of 1,204,840 rows via `pivot_longer()`. Cross-validating aggregated MYEB2 against MYEB3 showed **zero absolute differences** across all 19 years and 5 variables.

> Raw data files are not included in this repository. Download the MYEB1-MYEB3 CSVs from the [ONS website](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland?) and place them in a local `data/` folder before running the script.

## Methodology

The analysis follows five stages:

1. **Data integration & cleaning** - importing and standardising the three ONS datasets into a single analytical panel; recoding sex codes, reshaping wide-to-long, handling structural gaps (`Unattrib` variable from 2012 onward) and genuine outliers (e.g. City of London, 48.6% growth)
2. **Descriptive analytics** - population trends, growth rates, and regional comparisons across 331 local authorities, 2001 - 2020
3. **Hypothesis testing** - nine formal statistical tests examining demographic divergence between England and Wales, sexes, and growth classes
4. **Predictive modelling**
   - *Regression* (predicting population growth rate): OLS, Random Forest, XGBoost
   - *Clustering* (demographic typologies): K-means (k=4), validated with hierarchical clustering and PCA
   - *Classification* (growth class): multinomial logistic regression, Random Forest, XGBoost
5. **Critical evaluation** - comparing tool/method trade-offs and interpretability for policy use

## Key Results

**Population growth, 2001 – 2020:** England & Wales grew from 52.4M to 59.7M (+14.1%). England grew at ~1.6x the rate of Wales (14.4% vs 8.9%). Growth accelerated during 2004 – 2010 following EU expansion, then slowed after 2017. International net migration accounted for 56.6% of total growth.

**Demographic typology across 331 local authorities:**

| Type | Local Authorities | Share |
|---|---|---|
| Dual Growth | 182 | 55% |
| Migration Offsets Natural Decline | 105 | 32% |
| Natural Growth Offsets Out-migration | 41 | 12% |
| Dual Decline | 3 | 1% |

**Hypothesis testing:** 8 of 9 tests rejected H₀. Only the test comparing country against main growth driver (chi-square, χ²(1) = 0.221, p = 0.638) failed to reject.

**Regression (population growth rate):**

| Model | MAE | RMSE | R² |
|---|---|---|---|
| OLS | **2.59** | 3.81 | 0.749 |
| Random Forest | 3.04 | **3.65** | 0.735 |
| XGBoost | 2.77 | 3.71 | **0.761** |

OLS was recommended for policy use despite not winning every metric, due to its interpretability. Migration rate, death rate and birth rate were the strongest recurring predictors across all models.

**Classification (growth class - Low/Medium/High):**

| Model | Accuracy | Macro F1 |
|---|---|---|
| Multinomial Logistic Regression | **80.3%** | **0.800** |
| Random Forest | 76% | 0.76 |
| XGBoost | 38% | 0.38 (likely encoding/tuning issue) |

**Clustering (K-means, k=4):**

| Cluster | n | Label | Growth |
|---|---|---|---|
| 1 | 94 | Mixed Profile | 17.3% |
| 2 | 132 | Low-growth Suburban | 10.1% |
| 3 | 22 | High-growth Urban | 26.0% |
| 4 | 83 | Ageing High-Dependency | 11.3% |

## Limitations

- Small training set (n=267) limits ensemble model generalisation
- No spatial modelling - geographic patterns unaccounted for
- Random Forest showed some overfitting (train R² 0.97 vs test R² 0.74)

## Repository Structure

```
├── population_dynamics_analysis.R      # Full analysis script
├── Population_Dynamics_Summary.pdf     # Slide-deck summary of methodology and results
├── README.md
└── data/                               # (not included - see Data section)
```

## Tools & Packages

R 4.3, with `tidyverse`, `janitor`, `skimr`, `naniar`, `broom`, `car`, `caret`, `randomForest`, `xgboost`, `nnet`, `cluster`, `factoextra`, `scales`, `corrplot`

## Author

Hakeem Ololade Safiriyu (MSc)
[LinkedIn](www.linkedin.com/in/hakeem-safiriyu-1b2534386)
