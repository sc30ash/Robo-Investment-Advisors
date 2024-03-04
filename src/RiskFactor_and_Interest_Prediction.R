install.packages("rpart")
install.packages("caret")
install.packages("partykit")
install.packages("randomForest")
install.packages("glmnet")
install.packages("xgboost")

#data loading
riskdata=read.csv("C:\\Users\\jakki\\Downloads\\RiskAppetiteDataset.csv")
riskdata$Amount.needed= as.numeric(gsub("\\$|,", "", riskdata$Amount.needed))
library(rpart)
library(caret)
library(partykit)
library(randomForest)
library(dplyr)
library(glmnet)
library(xgboost)

#cleaning the dataset
clean_data<- riskdata%>% mutate(risk=factor(risk,levels=c(1,2,3),labels=c("Low","Medium","High")))
print(clean_data)
#splitting the data
set.seed(123)  # For reproducibility

# Shuffle the row indices
shuffled_indices <- sample(nrow(riskdata))

# Use createDataPartition with shuffled indices
train_proportion <- 0.8
train_size <- round(nrow(riskdata) * train_proportion)
train_indices <- createDataPartition(riskdata$risk[shuffled_indices], times = 1, p = train_proportion, list = TRUE)

# Extract the shuffled indices for the training set
train_data <- clean_data[shuffled_indices[train_indices[[1]]], ]
test_data <- clean_data[-shuffled_indices[train_indices[[1]]], ]

#training the models
train_decision_tree_model<- ctree(risk~age+monthly_income+marital_status+no_of_dependents,clean_data)

train_random_Forest_model<-randomForest(risk~age+monthly_income+marital_status+no_of_dependents,clean_data,type="classification")

#predicting the models
decision_tree_pred<- predict(train_decision_tree_model,newdata=test_data,type="response")

random_forest_pred<- predict(train_random_Forest_model,newdata=test_data)

#checking the accuracy
decision_tree_accuracy=mean(decision_tree_pred==test_data$risk)

random_forest_accuracy=mean(random_forest_pred==test_data$risk)

cat("decision tree accuracy is", decision_tree_accuracy,"\n")
cat("random forest accuracy is", random_forest_accuracy)


plot(train_decision_tree_model)
plot(train_random_Forest_model)


#creating a linear regression model
set.seed(142)

#splitting the dataset
training_data=riskdata[shuffled_indices[train_indices[[1]]], ]
testing_data=riskdata[-shuffled_indices[train_indices[[1]]], ]

#training the regression model
trained_linear_model= lm(interest~ risk+Maturity.Years+Amount.needed,training_data)

#predicting the model
linear_prediction=predict(trained_linear_model,testing_data)

rmse_lr <- sqrt(mean((linear_prediction - testing_data$interest)^2))

print(rmse_lr)
#Performing ridge regression
train_response=training_data$interest
train_predictors=as.matrix(training_data[,-which(names(training_data)=="interest")])
#cross validation
ridge=cv.glmnet(train_predictors,train_response, alpha=0)
best_lambda_in_ridge=ridge$lambda.min

ridge_model=glmnet(train_predictors,train_response,alpha=0,lambda=best_lambda_in_ridge)
test_response=testing_data$interest
test_predictors=as.matrix(testing_data[,-which(names(training_data)=="interest")])
predictions_from_ridge=predict(ridge_model,test_predictors,s=best_lambda_in_ridge)
ridge_test_error= mean((predictions_from_ridge- test_response)^2)

#performing lasso regression
lasso=cv.glmnet(train_predictors,train_response, alpha=1)
best_lambda_in_lasso=lasso$lambda.min
lasso_model=glmnet(train_predictors,train_response,alpha=1,lambda=best_lambda_in_lasso)
predictions_from_lasso=predict(lasso_model,test_predictors,s=best_lambda_in_lasso)
lasso_test_error= mean((predictions_from_lasso- test_response)^2)

#performing XG boost
dtrain <- xgb.DMatrix(data = as.matrix(training_data[, c("risk","Maturity.Years","Amount.needed")]), label = training_data$interest)

#parameters
params <- list(
  objective = "reg:squarederror",  # for regression tasks
  booster = "gbtree",              # use tree-based models
  eval_metric = "rmse"             # root mean squared error as the evaluation metric
)

# Train the model
xgb_model <- xgboost(data = dtrain, params = params, nrounds = 100)

#predicitng the model
dtest <- xgb.DMatrix(data = as.matrix(testing_data[,c("risk","Maturity.Years","Amount.needed")]))
predictions <- predict(xgb_model, dtest)



error <- sqrt(mean((predictions - test_data$interest)^2))


#Random Forest model
rf_model <- randomForest(
  formula = interest ~ risk+Maturity.Years+Amount.needed,
  data = training_data,
  ntree = 10,         # Number of trees in the forest
  mtry = sqrt(ncol(train_data)),  # Number of variables randomly sampled as candidates at each split
  importance = TRUE    # Calculate variable importance
)

# Make predictions on the test set
rfpredictions <- predict(rf_model, newdata = testing_data)



# Evaluate the model
rferror <- sqrt(mean((rfpredictions - test_data$interest)^2))



cat("Linear regression test error: ", rmse_lr,"\n")
cat("Ridge regreeion test error: ",ridge_test_error,"\n")
cat("Lasso regression test error: ",lasso_test_error)
cat("XG Boost test Error: ", error, "\n")
cat("RAndom Forest model test error: ", rferror, "\n")

