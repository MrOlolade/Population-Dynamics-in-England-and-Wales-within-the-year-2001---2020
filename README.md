# Population Dynamics in England & Wales Within The Year 2001 - 2020

A data analytics study of demographic change across 331 local authorities in England and Wales, built in R as part of an MSc Data Science project at the University of Gloucestershire.

## Overview

This project integrates three ONS Mid-Year Population Estimates datasets into a structured analytical panel, then applies statistical testing, regression, clustering and classification to understand how and why populations grew differently across regions between 2001 and 2020.

**Headline finding:** The combined population of England and Wales grew from 52.4M to 59.7M (+14.1%) over the period, with international net migration accounting for 56.6% of that growth. England grew almost twice as fast as Wales (14.4% vs 8.9%).

## Data

Three ONS datasets are used:

| Dataset | Description |
|---|---|
| MYEB1 | Detailed population estimates series (UK, 2020 geography) |
| MYEB2 | Detailed components of change series (England & Wales, 2020 geography) - primary analytical dataset |
| MYEB3 | Summary components of change series (UK, 2020 geography) |

> Raw data files are not included in this repository. Download the MYEB1-MYEB3 CSVs from the [ONS website](https://www.ons.gov.uk/) and place them in a local `data/` folder before running the script.

## Methodology

The analysis follows five stages:

1. **Data integration & cleaning** - importing and standardising the three ONS datasets into a single analytical panel
2. **Descriptive analytics** - population trends, growth rates, and regional comparisons across 331 local authorities, 2001 - 2020
3. **Hypothesis testing** - nine formal statistical tests examining demographic divergence between England and Wales, sexes, and growth classes
4. **Predictive modelling**
   - *Regression* (predicting population growth rate): OLS, Random Forest, XGBoost
   - *Clustering* (demographic typologies): K-means (k=4), validated with hierarchical clustering and PCA
   - *Classification* (growth class): multinomial logistic regression, Random Forest, XGBoost
5. **Critical evaluation** - comparing tool/method trade-offs and interpretability for policy use

## Key Results

**Regression (population growth rate):**

| Model | MAE | RMSE | R² |
|---|---|---|---|
| OLS | 2.59 (best) | - | - |
| Random Forest | - | 3.65 (best) | - |
| XGBoost | - | - | 0.761 (best) |

OLS was recommended for policy use despite not winning every metric, due to its interpretability.

**Classification (growth class):**

| Model | Accuracy | Macro F1 |
|---|---|---|
| Multinomial Logistic Regression | 80.3% (best) | 0.800 (best) |
| Random Forest | - | - |
| XGBoost | - | - |

**Clustering:** K-means (k=4) identified distinct demographic typologies across local authorities, validated against hierarchical clustering and PCA.

## Repository Structure

```
├── population_dynamics_analysis.R   # Full analysis script
├── README.md
└── data/                            # (not included - see Data section)
```

## Tools & Packages

R 4.3, with `tidyverse`, `janitor`, `skimr`, `naniar`, `broom`, `car`, `caret`, `randomForest`, `xgboost`, `nnet`, `cluster`, `factoextra`, `scales`, `corrplot`

## Author

Hakeem Ololade Safiriyu - MSc Data Science, University of Gloucestershire
[LinkedIn](www.linkedin.com/in/hakeem-safiriyu-1b2534386)
