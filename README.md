# Predicting the Potential Spread of Forest Fires Using Climate Data

## Introduction
Forest fires are natural disasters that can cause immense damage to both the environment and human lives. With the anticipated increase in frequency and intensity of wildfires due to climate change, accurately predicting the potential spread of forest fires is more critical than ever. This project investigates the factors influencing the Initial Spread Index (ISI), a key measure of how quickly a fire might spread, using data from Montesinho Natural Park in Portugal. The analysis was conducted using R, with packages such as `ggplot2` and `ggpubr` for data visualization, `GGally` for exploratory data analysis, `glmnet` for LASSO regression, and `car` for regression diagnostics, enabling us to build and evaluate predictive models for forest fire spread.

## Purpose
The main objective of this project is to identify the critical environmental factors that affect the spread of forest fires and to develop a predictive model for the Initial Spread Index (ISI) using these factors. By doing so, we aim to enhance early warning systems and improve strategies for wildfire management and prevention.

## Significance
Wildfires represent a significant threat to ecosystems, human life, and infrastructure. As global temperatures rise, understanding and predicting the conditions that lead to the rapid spread of wildfires becomes increasingly important. This project contributes valuable insights into wildfire dynamics, offering practical tools that can be used by disaster response teams and policymakers to anticipate and mitigate the effects of wildfires.

## Project Overview
### Data Source:
Data was collected from Montesinho Natural Park in Portugal between January 2000 and December 2003, covering environmental variables such as temperature, wind speed, relative humidity, and season.

### Exploratory Data Analysis:
`GGally` was used for scatter plots and correlation matrices, while `ggplot2` and `ggpubr` were utilized for box plots and visual analysis of relationships between ISI and predictor variables.

### LASSO Regression:
Variable selection was performed using LASSO regression with the `glmnet` package to identify significant predictors.

### Model Diagnostics:
The `car` package was used to assess regression diagnostics and ensure the model’s assumptions were met.

### Model Evaluation:
The dataset was split into training and validation sets to evaluate the model's performance, using `ggplot2` and `ggfortify` for visualization.

### Outlier Analysis:
During the regression analysis, outliers were identified by examining leverage and studentized residuals. Outliers were removed to improve model accuracy and ensure robust predictions.

## Findings and Conclusions
### Key Predictors: 
Temperature, wind speed, and season were identified as significant predictors of the Initial Spread Index (ISI). The final model explained a substantial portion of the variance in ISI, with an adjusted R-squared value of approximately 60%.

### Model Performance: 
While the model performed well in predicting lower ISI values in the training dataset, its predictive accuracy for higher ISI values in the validation dataset was lower, indicating a need for further refinement.

### Limitations: 
The model’s applicability may be limited by the dataset's geographic and temporal scope, which could affect the generalizability of the results to other regions or current conditions.

## References
This project utilized R for statistical analysis, leveraging packages like `ggplot2`, `glmnet`, and `GGally` for comprehensive data analysis and visualization. Full code and documentation are available in the repository.


