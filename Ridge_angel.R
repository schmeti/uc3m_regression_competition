### Ridge Regression Example with Prediction
library(glmnet)
library(readxl)
library(dplyr)

# Read and preprocess the training and test datasets
data_train <- read_excel("Data/data_train.xlsx") %>% preprocess()
data_test <- read_excel("Data/data_test_tryout.xlsx") %>% preprocess()
data_test$test_indices = NULL

# Generate regression matrix for training
X_train <- model.matrix(y ~ . - 1, data = data_train)

# Train Ridge Regression model (alpha = 0)
fit.ridge <- glmnet(X_train, data_train$y, alpha = 0)

# Cross-validation to determine optimal lambda
cv.out <- cv.glmnet(X_train, data_train$y, alpha = 0)  # alpha=0 for Ridge
opt_lambda <- cv.out$lambda.min

# Generate regression matrix for testing (no response variable needed)
X_test <- model.matrix(~ . - 1, data = data_test)  # Notice `y` is not included

# Predict using the Ridge model
predicted_values <- predict(fit.ridge, newx = X_test, type = "response", s = opt_lambda)

# Output predicted values
print(predicted_values)

# Optionally save predicted values
write.csv(predicted_values, "predicted_prices.csv", row.names = FALSE)



k_fold_cv_ridge <- function(data_train, k = 4, alpha = 0) {
  cat("=== Running k_fold Cross Validation --- Ridge === \n")
  
  # Create the K-fold partition
  folded_data <- k_fold(data_train, k)$.folds
  
  # Initialize vectors to store metrics
  cv_rmse_y <- numeric(k)
  cv_rmse_logy <- numeric(k)
  cv_rsq_adj_logy <- numeric(k)
  
  X = model.matrix(y ~ ., data = data_train)
  y = data_train$y
  opt_lambda <- cv.glmnet(X, y, alpha = alpha)$lambda.min
  
  for (i in 1:k) {
    # Create the fold's test/train split
    temp_train <- data_train[which(folded_data != i), ]
    temp_test <- data_train[which(folded_data == i), ]
    
    # Prepare design matrices
    X_train <- model.matrix(y ~ . - 1, data = temp_train)  # Exclude intercept
    y_train <- temp_train$y
    X_test <- model.matrix(y ~ . - 1, data = temp_test)
    y_test <- temp_test$y
    
    # Fit Ridge model
    ridge_model <- glmnet(X_train, y_train, alpha = alpha)
    
    # # Find optimal lambda using cross-validation
    # cv_ridge <- cv.glmnet(X_train, y_train, alpha = alpha)
    # opt_lambda <- cv_ridge$lambda.min
    
    # Predict on test data
    predictions <- predict(ridge_model, newx = X_test, s = opt_lambda)
    
    ## Calculate error metrics and store them
    
    # RMSE in log(y)
    n_test <- nrow(temp_test)
    SSE <- sum((y_test - predictions)^2)
    cv_rmse_logy[i] <- sqrt(SSE / n_test)
    
    # RMSE in y
    SSE_exp <- sum((exp(y_test) - exp(predictions))^2)
    cv_rmse_y[i] <- sqrt(SSE_exp / n_test)
    
    # Adjusted R-squared in log(y)
    num_predictors <- ncol(X_train)  # Number of predictors
    SST <- sum((mean(y_test) - y_test)^2)
    cv_rsq_adj_logy[i] <- 1 - (SSE / (n_test - num_predictors)) / (SST / (n_test - 1))
  }
  
  # Return the vector with metrics for each fold and their means
  return(list(cv_rmse_y = cv_rmse_y,
              mean_cv_rmse_y = mean(cv_rmse_y),
              cv_rmse_logy = cv_rmse_logy,
              mean_cv_rmse_logy = mean(cv_rmse_logy),
              cv_rsq_adj_logy = cv_rsq_adj_logy,
              mean_cv_rsq_adj_logy = mean(cv_rsq_adj_logy)))
}


# Assuming `data_train` is preprocessed and ready
results <- k_fold_cv_ridge(data_train = data_train)
print(results)

