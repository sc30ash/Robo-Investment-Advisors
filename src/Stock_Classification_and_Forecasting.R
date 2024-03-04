#Import Financial Data
library(readr)
#Install necessary packages and needed libraries
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(cowplot)
library(pROC)
library(ggcorrplot)
library(corrplot)
library(mlbench)
library(caret)
FinancialData = read_csv('2018_Financial_Data.csv')

head(FinancialData)

#Gather missing data and plot graph to visualize missing data within variables
missing_data <- FinancialData %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
missing_data2=subset(missing_data,missing_data$percent_missing>0.1)
ggplot(missing_data2, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()

#Delete These Variables From the Originial Dataset 
FinancialData2=subset(FinancialData,select = -c(43,53,76,77,78,82,83,84,85,87,88,90,91,96,97,99,100,101,103,105,112,114,120,127,128,129,130,132,143,146,148,149,150,151,152,153,155,159,172,173,174,175,186,201,202,204,205,207,208,210,211,213,214))


#Delete variable with singular value
table(FinancialData2$operatingProfitMargin)
FinancialData3=subset(FinancialData2,select = -c(80))

#Delete variables that are heavily correlated i.e. EPS vs EPS diluted etc.
FinancialData4=subset(FinancialData3,select = -c(15,19,21,33,34,38,45,47,49,50,51,94,103,126,150))

#Clean up variable names to more easily work with them 
library(janitor)
FinancialData4<-FinancialData4 %>% clean_names()
colnames(FinancialData4)

#Eliminate Market.Cap (numerical) variable and replace it with a categorical variable
market_cap_cat<-cut(FinancialData4$market_cap,breaks = c(0,1e+9,1e+10,1e+20),labels =c("Small","Mid","Large") )
FinancialData4$market_cap_cat=market_cap_cat
table(market_cap_cat)
FinancialData5=subset(FinancialData4,select = -c(100,138,136))

#Perform necessary structural adjustments to the data
##str(FinancialData5)
FinancialData5$x1=as.character(FinancialData5$x1)
FinancialData5$market_cap_cat=as.factor(FinancialData5$market_cap_cat)
FinancialData5$class=as.factor(FinancialData5$class)
FinancialData5$sector=as.factor(FinancialData5$sector)

#Run a Linear Regression to find and eliminate colinearity/non-essential variables 
fit01 = lm(x2019_price_var_percent~.- x1 - sector - class - market_cap_cat,FinancialData5)
summary(fit01)

#Create new dataset elminating "N/A" variables
FinancialData6=subset(FinancialData5,select = -c(11,18,23,28,29,67,68,86,87,88,93,94,100,101,102,104,105,107,110,124,125,126,128,129))

pe_ratio_list <- FinancialData5[, c('x1','price_earnings_ratio')]                     

head(FinancialFinal)

#Create a new variable to categorize price variance into broader baskets 
summary(FinancialData4$x2019_price_var_percent)
AnalystRating<-cut(FinancialData4$x2019_price_var_percent,breaks = c(-100,-50,-7.477,17.639,60,4000),labels =c("Strong Sell","Sell","Hold","Buy","Strong Buy") )
AnalystRating[1:10]
FinancialData6$AnalystRating=AnalystRating

#Use tree based algorithim r-part to determine the most important variables in the dataset
rpartMod<-train(AnalystRating~.- x1 - sector - class - market_cap_cat-x2019_price_var_percent,data = FinancialData6,method="rpart",na.action = na.exclude)
rpartImp<-varImp(rpartMod)
print(rpartImp)

#Remove more heavily correlated and redundant variabels 
FinancialData7=subset(FinancialData6,select = -c(85,98,99,101,9,23,20,24,88,65,91,61,84,79,82,36,118,62,19,22,53,15,100))

#Re-run rpart model
rpartMod<-train(AnalystRating~.- x1 - sector - class - market_cap_cat-x2019_price_var_percent,data = FinancialData7,method="rpart",na.action = na.exclude)
rpartImp<-varImp(rpartMod)
print(rpartImp)

#Put selected variables into a dataset  
##colnames(FinancialData7)
VariableCorr2=subset(FinancialData7,select = c(14,15,25,52,53))
colnames(VariableCorr2)
summary(VariableCorr2)

#Check that selected variables are not heavily correlated 
##colnames(FinancialData7)
corr2<-round(cor(VariableCorr2),1)
ggcorrplot(corr2,lab = TRUE)

#Combine important numerical variables found through rpart with important categorical variables for final dataset
FinancialFinal=subset(FinancialData7,select = c(1,14,15,25,52,53,104,105,106,107,108))
summary(FinancialFinal)
colnames(FinancialFinal)

#Replace missing numerical values to take care of outliers and clean up distribution
Q<-quantile(FinancialFinal$eps, probs = c(.15,.85),na.rm = TRUE)
iqr<-IQR(FinancialFinal$eps, na.rm = TRUE)
up<-Q[2]+1.5*iqr 
low<-Q[1]-1.5*iqr
max(FinancialFinal$eps)
eliminated<-subset(FinancialFinal,FinancialFinal$eps>(Q[1]-1.5*iqr) & FinancialFinal$eps < (Q[2]+1.5*iqr))

Q2<-quantile(eliminated$dividend_per_share, probs = c(.01,.99),na.rm = TRUE)
iqr2<-IQR(eliminated$dividend_per_share, na.rm = TRUE)
up2<-Q[2]+1.5*iqr2
low2<-Q[1]-1.5*iqr2
eliminated2<-subset(eliminated,eliminated$dividend_per_share>(Q[1]-1.5*iqr2) & eliminated$dividend_per_share < (Q[2]+1.5*iqr2))

Q3<-quantile(eliminated2$total_assets, probs = c(.01,.99),na.rm = TRUE)
iqr3<-IQR(eliminated2$total_assets,na.rm = TRUE)
up3<-Q[2]+1.5*iqr3
low3<-Q[1]-1.5*iqr3
eliminated3<-subset(eliminated2,eliminated2$total_assets>(Q[1]-1.5*iqr3) & eliminated2$total_assets < (Q[2]+1.5*iqr3))

Q4<-quantile(eliminated3$net_profit_margin_2, probs = c(.01,.99),na.rm = TRUE)
iqr4<-IQR(eliminated3$net_profit_margin_2, na.rm = TRUE)
up4<-Q[2]+1.5*iqr4
low4<-Q[1]-1.5*iqr4
eliminated4<-subset(eliminated3,eliminated3$net_profit_margin_2>(Q[1]-1.5*iqr4) & eliminated3$net_profit_margin_2 < (Q[2]+1.5*iqr4))

Q5<-quantile(eliminated4$return_on_equity, probs = c(.01,.99),na.rm = TRUE)
iqr5<-IQR(eliminated4$return_on_equity, na.rm = TRUE)
up5<-Q[2]+1.5*iqr5
low5<-Q[1]-1.5*iqr5
eliminated5<-subset(eliminated4,eliminated4$return_on_equity>(Q[1]-1.5*iqr5) & eliminated4$return_on_equity < (Q[2]+1.5*iqr5))
summary(eliminated5)
str(eliminated5)

#Replace missing values of numeric variables
eliminated5$eps[is.na(eliminated5$eps)] <- round(median(eliminated5$eps, na.rm = TRUE))
eliminated5$dividend_per_share[is.na(eliminated5$dividend_per_share)] <- round(median(eliminated5$dividend_per_share, na.rm = TRUE))
eliminated5$total_assets[is.na(eliminated5$total_assets)] <- round(median(eliminated5$total_assets, na.rm = TRUE))
eliminated5$net_profit_margin_2[is.na(eliminated5$net_profit_margin_2)] <- round(median(eliminated5$net_profit_margin_2, na.rm = TRUE))
eliminated5$return_on_equity[is.na(eliminated5$return_on_equity)] <- round(median(eliminated5$return_on_equity, na.rm = TRUE))
summary(eliminated5)


#Scale total assets due to large numeric values 
eliminated5$total_assets<-scale(eliminated5$total_assets,scale = TRUE)
summary(eliminated5)

#Eliminate observations with missing market cap information 
FinancialFinal2<-subset(na.omit(eliminated5))
summary(FinancialFinal2)
str(FinancialFinal2)

#EDA of Categorical Variables 
library(repr)
options(repr.plot.width = 17, repr.plot.height = 10)
ggplot(FinancialFinal2, aes(x=sector,fill=class))+ geom_bar(position = 'fill')+theme_bw()
options(repr.plot.width = 10, repr.plot.height = 10)
ggplot(FinancialFinal2, aes(x=market_cap_cat,fill=class))+ geom_bar(position = 'fill')+theme_bw()
options(repr.plot.width = 17, repr.plot.height = 10)
ggplot(FinancialFinal2, aes(x=sector,fill=AnalystRating))+ geom_bar()+ theme_bw() 
options(repr.plot.width = 10, repr.plot.height = 10)       
ggplot(FinancialFinal2, aes(x=market_cap_cat,fill=AnalystRating))+ geom_bar(position = 'fill')+theme_bw()

#EDA of Numerical Variables
ggplot(FinancialFinal2, aes(x=AnalystRating, y=eps, fill=AnalystRating)) + geom_violin()+
  geom_boxplot(width=.1, fill="white") + labs(title="EPS") 

ggplot(FinancialFinal2, aes(x=AnalystRating, y=return_on_equity, fill=AnalystRating)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Total Return on Equity")

ggplot(FinancialFinal2, aes(x=AnalystRating, y=net_profit_margin_2, fill=AnalystRating)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Total Net Profit Margin")

ggplot(FinancialFinal2, aes(x=AnalystRating, y=total_assets, fill=AnalystRating)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Total Assets")

ggplot(FinancialFinal2, aes(x=AnalystRating, y=dividend_per_share, fill=AnalystRating)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Total Dividend Per Share")

#Pick the best model
control = trainControl(method="cv", number=10)
metric = "Accuracy"
# Linear Discriminant Analysis (LDA)
set.seed(138)
fit.lda = train(AnalystRating~.- x1 - class -x2019_price_var_percent, data=FinancialFinal2, method="lda", metric=metric, trControl=control,na.action = na.pass)

# Classfication and Regression Trees (CART)
set.seed(138)
fit.cart = train(AnalystRating~.- x1 - class -x2019_price_var_percent, data=FinancialFinal2, method="rpart", metric=metric, trControl=control)

# k-Nearest Neighbors (KNN)
set.seed(138)
fit.knn = train(AnalystRating~.- x1 - class -x2019_price_var_percent, data=FinancialFinal2, method="knn", metric=metric, trControl=control)

# Bayesian Generalized Linear Model 
set.seed(138)
fit.logi = train(AnalystRating~.- x1 - class -x2019_price_var_percent, data=FinancialFinal2, method="bayesglm", metric=metric, trControl=control)

# Support Vector Machines (SVM) 
set.seed(138)
fit.svm = train(AnalystRating~.- x1 - class -x2019_price_var_percent, data=FinancialFinal2, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(138)
fit.rf = train(AnalystRating~.- x1 - class -x2019_price_var_percent, data=FinancialFinal2, method="rf", metric=metric, trControl=control)

# Gradient Boosting Machines/XGBoost
set.seed(138)
fit.xgb = train(AnalystRating~.- x1 - class -x2019_price_var_percent, data=FinancialFinal2, method="xgbLinear", metric=metric, trControl=control)

# Select Best Model
# summarize accuracy of models
results = resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, logi=fit.logi, svm=fit.svm, rf=fit.rf,xgb=fit.xgb))
summary(results)

#Now that we know that we cannot predict the level of variance with much accuracy,
#Logit Regression using Class
fit02 = glm(class~.-x1 -AnalystRating-x2019_price_var_percent, data=FinancialFinal2, family = "binomial")
summary(fit02)

#Pick the best model
library(arm)
library(xgboost)
control = trainControl(method="cv", number=10)
metric = "Accuracy"
# Linear Discriminant Analysis (LDA)
set.seed(138)
fit.lda2 = train(class~.-x1 -AnalystRating-x2019_price_var_percent, data=FinancialFinal2, method="lda", metric=metric, trControl=control,na.action = na.pass)

# Classfication and Regression Trees (CART)
set.seed(138)
fit.cart2 = train(class~.-x1 -AnalystRating-x2019_price_var_percent, data=FinancialFinal2, method="rpart", metric=metric, trControl=control)

# k-Nearest Neighbors (KNN)
set.seed(138)
fit.knn2 = train(class~.-x1 -AnalystRating-x2019_price_var_percent, data=FinancialFinal2, method="knn", metric=metric, trControl=control)

# Bayesian Generalized Linear Model 
set.seed(138)
fit.logi2 = train(class~.-x1 -AnalystRating-x2019_price_var_percent, data=FinancialFinal2, method="bayesglm", metric=metric, trControl=control)

# Support Vector Machines (SVM) 
set.seed(138)
fit.svm2 = train(class~.-x1 -AnalystRating-x2019_price_var_percent, data=FinancialFinal2, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(138)
fit.rf2 = train(class~.-x1 -AnalystRating-x2019_price_var_percent, data=FinancialFinal2, method="rf", metric=metric, trControl=control)

# Gradient Boosting Machines/XGBoost
set.seed(138)
fit.xgb2 = train(class~.-x1 -AnalystRating-x2019_price_var_percent, data=FinancialFinal2, method="xgbLinear", metric=metric, trControl=control)

# Select Best Model
# summarize accuracy of models
results2 = resamples(list(lda2=fit.lda2, cart2=fit.cart2, knn2=fit.knn2, logi2=fit.logi2, svm2=fit.svm2, rf2=fit.rf, xgb2=fit.xgb2))
summary(results2)
summary(fit.xgb2)

#Find Optimal Cutoff Threshold, AUC, & Create a Confusion Matrix
library(pROC)
library(caTools)
pred = predict(fit.xgb2, type = "prob", FinancialFinal2)
pred.1 = as.numeric(pred[,2])
xgb.roc = roc(response = FinancialFinal2$class, predictor = pred.1)
plot(xgb.roc, legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
coords(xgb.roc, "best", "threshold", transpose = TRUE)

indices = sample.split(FinancialFinal2$class, SplitRatio = 0.3) #split sample with this ratio
train = FinancialFinal2[indices,]
test = FinancialFinal2[!indices,]
pred = predict(fit.xgb2, test)
confusionMatrix(pred,test$class,positive = "1")

# Save model object to a file
save(fit.xgb2, file = "stock_classification.RData")

load("stock_classification.RData")

test$predictions = predict(fit.xgb2, test)
train$predictions = predict(fit.xgb2, train)

predictions = predict(fit.xgb2, type = "prob", FinancialFinal2)

FinancialFinal2$pred0 <- predictions[,"0"]
FinancialFinal2$pred1 <- predictions[,"1"]
FinancialFinal2$final_predictions <- ifelse(FinancialFinal2$pred0 > FinancialFinal2$pred1, 0, 1)

merged_data <- merge(FinancialFinal2, pe_ratio_list, by.x = "x1", by.y = "x1", all.x = TRUE)
FinancialFinal2$price_earnings_ratio <- merged_data$price_earnings_ratio

# Install required packages
install.packages("keras")
install.packages("quantmod")

# Load libraries
library(quantmod)
library(keras)

# Function to predict stock prices using LSTM
predict_stock_prices_quarterly <- function(stock_prices) {
  # Assuming stock_prices is a data frame with columns 'Date' and 'Close'
  
  # Convert 'Date' to Date type
  stock_prices$Date <- as.Date(stock_prices$Date)
  
  # Convert daily data to quarterly data
  stock_prices_quarterly <- to.quarterly(stock_prices, OHLC = FALSE)
  
  # Extract and normalize 'Close' prices
  close_prices <- stock_prices_quarterly$Close
  normalized_prices <- (close_prices - min(close_prices)) / (max(close_prices) - min(close_prices))
  
  # Use only the last 5-6 years of data
  start_index <- max(1, length(normalized_prices) - 6 * 4)
  normalized_prices <- normalized_prices[start_index:length(normalized_prices)]
  
  # Create training data
  window_size <- 4  # Quarterly data
  x_train <- matrix(0, nrow = length(normalized_prices) - window_size, ncol = window_size)
  y_train <- normalized_prices[(window_size + 1):length(normalized_prices)]
  
  for (i in 1:(length(normalized_prices) - window_size)) {
    x_train[i, ] <- normalized_prices[i:(i + window_size - 1)]
  }
  
  # Reshape data for LSTM input (samples, time steps, features)
  x_train <- array(x_train, dim = c(dim(x_train), 1))
  
  # Build LSTM model
  model <- keras_model_sequential()
  model %>%
    layer_lstm(units = 100, input_shape = c(window_size, 1), return_sequences = TRUE) %>%
    layer_lstm(units = 100, return_sequences = TRUE) %>%
    layer_lstm(units = 50) %>%
    layer_dense(units = 1)
  
  # Compile the model
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam(learning_rate = 0.0005),  # Adjust learning rate
    metrics = c('mean_absolute_error')  # Monitor mean absolute error during training
  )
  
  # Train the model
  model %>% fit(x_train, y_train, epochs = 150, batch_size = 32, validation_split = 0.1)  # Use validation set
  
  # Predict the next 4 quarters of stock prices
  future_prices <- matrix(0, nrow = 4, ncol = window_size)
  
  for (i in 1:4) {
    future_prices[i, ] <- normalized_prices[(length(normalized_prices) - window_size + 1):length(normalized_prices)]
    predicted_price <- model %>% predict(array(future_prices[i, , drop = FALSE], dim = c(1, window_size, 1)))
    normalized_prices <- c(normalized_prices, predicted_price)
  }
  
  # Denormalize the predicted prices
  predicted_prices <- predicted_price * (max(close_prices) - min(close_prices)) + min(close_prices)
  
  return(predicted_prices)
}

getTickerReturns <- function(ticker){
  getSymbols(ticker, from = "2010-01-01", to = "2018-01-01", auto.assign = TRUE)
  stock_prices <- data.frame(Date = index(AAPL), Close = as.numeric(Cl(AAPL)))
  
  # Predict stock prices quarterly
  predicted_prices <- predict_stock_prices_quarterly(stock_prices)
  
  # Calculate CAGR over the next 1 year (4 quarters)
  initial_investment <- stock_prices$Close[nrow(stock_prices)]
  final_value <- predicted_prices[length(predicted_prices)]
  cagr <- ((final_value / initial_investment)^(1/4)) - 1
  
  return(cagr)
}

returns_list <- c()
for (ticker in FinancialFinal2$x1) {
  cagr <- getTickerReturns(ticker)
  returns_list[length(returns_list)+1] <- cagr
}
FinancialFinal2$returns <- returns_list

# Load libraries
library(openxlsx)

# Filter rows where predictions is equal to 1 and market_cap_cat is either "Large" or "Mid"
filtered_data <- FinancialFinal2[FinancialFinal2$final_predictions == 1 & FinancialFinal2$price_earnings_ratio > 0 & FinancialFinal2$market_cap_cat %in% c("Large"), c("x1", "price_earnings_ratio", "returns")]

# Write filtered_data to Excel
write.xlsx(filtered_data, file = "good_stock_tickers.xlsx", row.names = FALSE)
