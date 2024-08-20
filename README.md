# Health-Care-Case-Study


This repository contains an analysis of a healthcare dataset using various statistical methods, including exploratory data analysis (EDA), hypothesis testing, clustering, predictive modeling, and survival analysis.

## Table of Contents
- [Installation](#installation)
- [Data Preprocessing](#data-preprocessing)
- [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
- [Hypothesis Testing](#hypothesis-testing)
- [Clustering Analysis](#clustering-analysis)
- [Principal Component Analysis (PCA)](#principal-component-analysis-pca)
- [Predictive Analysis](#predictive-analysis)
- [Survival Analysis](#survival-analysis)

## Installation

To reproduce the analysis, you need to install the required R packages. Run the following commands in R:

```r
install.packages("survminer")
install.packages("factoextra")
install.packages("ROSE")
install.packages("randomForest")

Data Preprocessing
The dataset is in CSV format and is loaded using read.csv.
Categorical variables are converted into factors.
Date columns are converted to Date format.
The length of hospital stay is calculated by subtracting the admission date from the discharge date.
Missing values are handled by removing rows with NA values using na.omit.


Exploratory Data Analysis (EDA)
Summary Statistics: A summary of the dataset is generated.

Visualizations:
Age distribution of patients using a histogram.
Billing amount by admission type using boxplots.
Length of stay by medical condition using boxplots.

Hypothesis Testing
A t-test is conducted to check if there's a significant difference in billing amounts between emergency and elective admissions.
A Chi-square test is used to examine the relationship between medical conditions and test results.

Clustering Analysis
K-means Clustering: Performed on the Age and Billing.Amount variables.
Clusters are visualized using fviz_cluster from the factoextra package.
Unscaled cluster centers are computed to interpret the clusters in the original scale of the data.

Principal Component Analysis (PCA)
PCA is performed on numeric variables to reduce dimensionality.
The PCA results are visualized with individuals colored based on the outcome variable.

Predictive Analysis
Random Forest: A random forest model is built with class weights to handle class imbalance. The model’s performance is evaluated using a confusion matrix.


Survival Analysis
Kaplan-Meier Estimator: Used to analyze the survival probability based on gender and medical condition.
Cox Proportional-Hazards Model: Fits a Cox model to examine the effect of covariates on survival time.

Conclusion: This analysis provides insights into various aspects of patient data, including demographics, billing, and survival probabilities. The combination of EDA, clustering, PCA, and predictive modeling offers a comprehensive understanding of the dataset.

