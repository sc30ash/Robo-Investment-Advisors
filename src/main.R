library(rpart)
library(caret)
library(partykit)
library(randomForest)
library(dplyr)
library(glmnet)
library(xgboost)

load("RateXGBoost.RData")
load("RiskRandomForest.RData")

calculate_principal <- function(final_amount, years, interest_rate, compound_frequency = 1) {
  interest_rate <- interest_rate/100
  principal = final_amount / ((1 + interest_rate / compound_frequency)^(compound_frequency * years))
  return(principal)
}

allocate_investment <- function(stock_names, coefficients, total_investment) {
  if(length(stock_names) != length(coefficients)) {
    stop("The number of stocks and coefficients must be the same.")
  }
  
  normalized_coefficients = coefficients / sum(coefficients)
  #Normalization of coefficients to sum upto 1
  
  investment_allocation = normalized_coefficients * total_investment
  #Allocation of investment based on normalized coefficients
  
  names(investment_allocation) = stock_names
  
  return(investment_allocation)
}

calculateRiskFactor <- function(age, marital_status, income, dependents){
  # Creating the data frame
  test_data <- data.frame(age = c(age), monthly_income = c(income), marital_status = c(marital_status), no_of_dependents = c(dependents))
  prediction <- predict(train_random_Forest_model,newdata=test_data)
  return(prediction[[1]])
}

calculateInterestRate <- function(risk_factor, maturity_years, amount_at_maturity){
  if(risk_factor == "High"){risk_factor <- 3}
  if(risk_factor == "Medium"){risk_factor <- 2}
  if(risk_factor == "Low"){risk_factor <- 1}
  test_data <- data.frame(risk = c(risk_factor), Maturity.Years = c(maturity_years), Amount.needed = c(amount_at_maturity))
  dtest <- xgb.DMatrix(data = as.matrix(test_data[,c("risk","Maturity.Years","Amount.needed")]))
  predictions <- predict(xgb_model, dtest)
  return(predictions[[1]])
}

library(R6)

# Define the Person class
Person <- R6Class(
  "Person",
  public = list(
    age = NULL,
    marital_status = NULL,
    name = NULL,
    monthly_income = NULL,
    num_dependents = NULL,
    risk_factor = NULL,
    
    initialize = function(age, marital_status, name, income, dependents) {
      self$age <- age
      self$marital_status <- marital_status
      self$name <- name
      self$monthly_income <- income
      self$num_dependents <- dependents
      
      self$risk_factor <- calculateRiskFactor(age, marital_status, income, dependents)
    },
    
    print_info = function() {
      cat("Name:", self$name, "\n")
      cat("Age:", self$age, "\n")
      cat("Marital Status:", self$marital_status, "\n")
    }
  )
)

# Define the Baskets class inheriting from Person
Baskets <- R6Class(
  "Baskets",
  inherit = Person,
  public = list(
    maturity_years = NULL,
    amount_at_maturity = NULL,
    interest_rate = NULL,
    initial_investment = NULL,
    
    initialize = function(person_obj, maturity_years, amount_at_maturity) {
      # Use the provided Person object
      self$age <- person_obj$age
      self$marital_status <- person_obj$marital_status
      self$name <- person_obj$name
      self$monthly_income <- person_obj$monthly_income
      self$num_dependents <- person_obj$num_dependents
      self$risk_factor <- person_obj$risk_factor
      
      self$maturity_years <- maturity_years
      self$amount_at_maturity <- amount_at_maturity
      self$interest_rate <- calculateInterestRate(self$risk_factor, maturity_years, amount_at_maturity)
      self$initial_investment <- calculate_principal(amount_at_maturity, maturity_years, self$interest_rate)
    },
    
    print_basket_info = function() {
      # Call the print_info method of the Person class
      super$print_info()
      
      cat("Maturity Years:", self$maturity_years, "\n")
      cat("Amount at Maturity:", self$amount_at_maturity, "\n")
      cat("Risk:", self$risk_factor, "\n")
      cat("Interest:", self$interest_rate, "\n")
      cat("initial_investment:", self$initial_investment, "\n")
    }
  )
)

# Create a Person object
john_doe <- Person$new(age = 30, marital_status = 0, name = "John Doe", income = 10000, dependents = 1)

# Create instances of the Baskets class reusing the same Person object
basket1 <- Baskets$new(person_obj = john_doe, maturity_years = 5, amount_at_maturity = 100000)
basket2 <- Baskets$new(person_obj = john_doe, maturity_years = 8, amount_at_maturity = 150000)

# Call methods to print information
basket1$print_basket_info()
basket2$print_basket_info()

stock_dataset <- read.csv('good_stock_tickers.csv')

# Sample data (replace with your actual data)
set.seed(123)
returns <- stock_dataset$returns
pe_ratio <- stock_dataset$price_earnings_ratio
y <- basket2$interest_rate

# Objective function to minimize
objective_function <- function(X, returns, pe_ratio, y) {
  ycap <- sum(X * returns)
  B <- sum(X * pe_ratio)
  l2_norm <- norm(as.matrix(X), type = "I")
  
  # Equation to minimize
  result <- (y - ycap)^2 + 10 / ((ycap - y) * B) + l2_norm + (1-sum(X))^2
  
  return(result)
}

# Initial guess for decision variables (replace with an appropriate initial guess)
initial_guess <- rep(0.1, length(returns))

# Minimize the objective function
result <- optim(
  par = initial_guess,
  fn = objective_function,
  returns = returns,
  pe_ratio = pe_ratio,
  y = y,
  method = "L-BFGS-B",
  lower = 0
)

# Optimal values of decision variables
optimal_X <- result$par

# Display the result
cat("Optimal X:", optimal_X, "\n")
cat("Objective Value:", result$value, "\n")

investments_by_stocks <- basket1$initial_investment*optimal_X
stocks <- stock_dataset$x1
investment_data <- data.frame(Stock = stocks, Investment = investments_by_stocks)
investment_data <- investment_data[investment_data$Investment > 0,]
# Display the DataFrame
print(investment_data)