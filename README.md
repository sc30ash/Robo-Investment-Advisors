# Robo-Investment-Advisors

# Project Readme

## Overview

This repository provides the final code and data files for our comprehensive investment management model. The project encompasses three main modules: risk factor and interest rate prediction, stock classification and forecasting, and the final example (RCode_FinalCode) demonstrating the complete model's functionality. Additionally, exploratory data analysis (EDA) code and data files, as well as individual model files, are included for reference and transparency.

Here is a brief overview of the workflow of this model.
![alt text](http://url/to/img.png)

## Files

### RCode_FinalCode

This script serves as a final example of our investment model, utilizing a placeholder person value for prediction. It incorporates the trained models, predicts, and displays the recommended investment amounts for each stock in every basket. The associated Rdata file (RData_FinalData) contains the relevant workspace.

### RCode_finalEDA

This script contains the exploratory data analysis (EDA) part referenced in the paper. The corresponding Rdata file (RData_FinalEDA) preserves the workspace for this analysis.

### Models

- **riskfactor_and_interest_prediction.R**: This script contains the code for modeling risk factor and interest predictions. The corresponding Rdata file stores the workspace for this module.

- **stock_classification_and_forecasting.R**: This script encompasses the code for training the stock classifier, evaluating the best model, and further training the predicted good stocks into LSTM models. The output is saved in an Excel file for later evaluation.

### Model Files

- **Risk Factor Model (riskfactor_model.Rdata)**
- **Interest Rate Predictor Model (interest_rate_model.Rdata)**
- **Stock Classification Model (stock_classification_model.Rdata)**

### Note

As each of the 3000 stocks has its own LSTM model, these individual models couldn't be included in the repository. Similarly, the final regression model, tailored for individual use cases, doesn't have a saving mechanism that aids in the repository. The provided Rdata files encapsulate the model workspaces for reference.

## Usage

For a comprehensive understanding of the project and model implementation, follow the code flow outlined in the provided scripts. The final example (RCode_FinalCode) demonstrates how to utilize the trained models for investment predictions.

Feel free to explore the EDA (RCode_finalEDA) to gain insights into the initial data exploration and analysis process.

## Dependencies

Ensure that the required libraries and dependencies mentioned in the code files are installed in your R environment. The code is designed to run seamlessly with the specified packages.
