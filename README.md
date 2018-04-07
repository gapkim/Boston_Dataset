# Comparison of model selection methods for Boston dataset (in ISLR package)

The performance of four selections methods, Best subsets, Ridge regression, Lasso regression, and Manual selection, have been compared using the ‘Boston’ dataset included in the MASS package. The model objective is to determine the relationship between per capita crime rate, ‘crim’, on other 13 predictors where 2 are categorical variables with 2 levels and 9 levels each. First, an exploratory data analysis is performed to analyze the distribution of variables and to investigate preliminary relationship between ‘crim’ and predictors. Due to high skewedness of variables leading to violations of linear regression assumptions, transformed variables are used throughout the model selection process. The Best subsets elminated 6 predictors and had the highest prediction accuracy followed by Manual selection method. The increase in model bias through Ridge or Lasso regression did not result in significant improvement in prediction accuracy for the transformed variables.

## File Description
Project1.html : Project report file (html) //
Project.Rmd : R code script
