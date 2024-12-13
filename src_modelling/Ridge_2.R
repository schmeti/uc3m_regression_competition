library(glmnet)
## Cross validation for Ridge (k-fold)
### K fold (default 10 folds) or LOO (folds = 736 (number of observations))
ridge_cv_no_interaction <- function(data, response_var = "y", k = 10) {
  set.seed(248)  # For reproducibility
  
  # Split data into folds
  folds <- sample(rep(1:k, length.out = nrow(data)))
  
  # Variables to store metrics
  rmse_train <- numeric(k)
  rmse_train_original <- numeric(k)
  rmse_test <- numeric(k)
  rmse_test_original <- numeric(k)
  r2_adj_train <- numeric(k)
  r2_adj_train_original <- numeric(k)
  r2_adj_test <- numeric(k)
  r2_adj_test_original <- numeric(k)
  coefficients_list <- list()  # Store coefficients for each fold
  
  # Perform cross-validation
  for (i in 1:k) {
    # Split into training and testing sets
    train_idx <- which(folds != i)
    test_idx <- which(folds == i)
    train_data <- data[train_idx, ]
    test_data <- data[test_idx, ]
    
    # Create design matrix with specific interactions and all other variables
    formula <- as.formula(paste(
      response_var,
      "~ . - 1"
    ))
    x_train <- model.matrix(formula, data = train_data)
    y_train <- train_data[[response_var]]
    x_test <- model.matrix(formula, data = test_data)
    y_test <- test_data[[response_var]]
    
    # Fit Ridge model with cross-validation to find optimal lambda
    cv_model <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 5)
    best_lambda <- cv_model$lambda.min
    
    # Fit final model with the best lambda
    ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)
    
    # Predictions on training and testing sets
    pred_train <- predict(ridge_model, newx = x_train)
    pred_test <- predict(ridge_model, newx = x_test)
    
    # Transform predictions and true values to the original scale
    pred_train_original <- exp(pred_train)
    pred_test_original <- exp(pred_test)
    y_train_original <- exp(y_train)
    y_test_original <- exp(y_test)
    
    # Calculate RMSE in logarithmic scale
    rmse_train[i] <- sqrt(mean((y_train - pred_train)^2))
    rmse_test[i] <- sqrt(mean((y_test - pred_test)^2))
    
    # Calculate RMSE in the original scale
    rmse_train_original[i] <- sqrt(mean((y_train_original - pred_train_original)^2))
    rmse_test_original[i] <- sqrt(mean((y_test_original - pred_test_original)^2))
    
    # Calculate adjusted R^2 in logarithmic scale
    sse_train <- sum((y_train - pred_train)^2)
    sst_train <- sum((y_train - mean(y_train))^2)
    r2_train <- 1 - (sse_train / sst_train)
    n_train <- length(y_train)
    p_train <- ncol(x_train)
    r2_adj_train[i] <- 1 - ((1 - r2_train) * (n_train - 1) / (n_train - p_train - 1))
    
    sse_test <- sum((y_test - pred_test)^2)
    sst_test <- sum((y_test - mean(y_test))^2)
    r2_test <- 1 - (sse_test / sst_test)
    n_test <- length(y_test)
    p_test <- ncol(x_test)
    r2_adj_test[i] <- 1 - ((1 - r2_test) * (n_test - 1) / (n_test - p_test - 1))
    
    # Calculate adjusted R^2 in the original scale
    sse_train_original <- sum((y_train_original - pred_train_original)^2)
    sst_train_original <- sum((y_train_original - mean(y_train_original))^2)
    r2_train_original <- 1 - (sse_train_original / sst_train_original)
    r2_adj_train_original[i] <- 1 - ((1 - r2_train_original) * (n_train - 1) / (n_train - p_train - 1))
    
    sse_test_original <- sum((y_test_original - pred_test_original)^2)
    sst_test_original <- sum((y_test_original - mean(y_test_original))^2)
    r2_test_original <- 1 - (sse_test_original / sst_test_original)
    r2_adj_test_original[i] <- 1 - ((1 - r2_test_original) * (n_test - 1) / (n_test - p_test - 1))
    
    # Store coefficients for each fold
    coefficients_list[[i]] <- coef(ridge_model)
  }
  
  # Return metrics and coefficients
  return(list(
    rmse_train = rmse_train,
    rmse_train_original = rmse_train_original,
    rmse_test = rmse_test,
    rmse_test_original = rmse_test_original,
    r2_adj_train = r2_adj_train,
    r2_adj_train_original = r2_adj_train_original,
    r2_adj_test = r2_adj_test,
    r2_adj_test_original = r2_adj_test_original,
    coefficients = coefficients_list
  ))
}

# Model Execution
k <- 4  # Number of folds, adjust as needed
ridge_results_k <- ridge_cv_no_interaction(data_train, response_var = "y", k = k)

# Output the results
cat("=== Ridge Regression with Optimal Lambda ===\n")
cat(sprintf("Average RMSE (Training, Logarithmic): %.3f\n", mean(ridge_results_k$rmse_train)))
cat(sprintf("Average RMSE (Training, Original): %.3f\n", mean(ridge_results_k$rmse_train_original)))
cat(sprintf("Average Adjusted R^2 (Training, Logarithmic): %.3f\n", mean(ridge_results_k$r2_adj_train)))
cat(sprintf("Average Adjusted R^2 (Training, Original): %.3f\n", mean(ridge_results_k$r2_adj_train_original)))

# Each fold
cat(sprintf("RMSE (Training, Logarithmic): %.3f\n", ridge_results_k$rmse_train))
cat(sprintf("RMSE (Training, Original): %.3f\n", ridge_results_k$rmse_train_original))
cat(sprintf("Adjusted R^2 (Training, Logarithmic): %.3f\n", ridge_results_k$r2_adj_train))
cat(sprintf("Adjusted R^2 (Training, Original): %.3f\n", ridge_results_k$r2_adj_train_original))

# Plot RMSE for training set (Original Scale) and Log scale
plot(1:k, ridge_results_k$rmse_train_original, type = "b", col = "blue", ylim = range(c(ridge_results_k$rmse_train_original, ridge_results_k$rmse_test_original)), xlab = "Fold", ylab = "RMSE (Original Scale)", main = "RMSE for Training Set")
plot(1:k, ridge_results_k$rmse_train, type = "b", col = "red", ylim = range(c(ridge_results_k$rmse_train, ridge_results_k$rmse_test)), xlab = "Fold", ylab = "RMSE (Log Scale)", main = "RMSE for Training Set")

# Plot Adjusted R^2 for training set (Original Scale) and Log Scale
plot(1:k, ridge_results_k$r2_adj_train_original, type = "b", col = "blue", ylim = range(c(ridge_results_k$r2_adj_train_original, ridge_results_k$r2_adj_test_original)), xlab = "Fold", ylab = "Adjusted R^2 (Original Scale)", main = "Adjusted R^2 for Training Set")
plot(1:k, ridge_results_k$r2_adj_train, type = "b", col = "red", ylim = range(c(ridge_results_k$r2_adj_train, ridge_results_k$r2_adj_test)), xlab = "Fold", ylab = "Adjusted R^2 (Log Scale)", main = "Adjusted R^2 for Training Set")


## Cross validation for Ridge (Leave One Out)
### K fold (default 10 folds) or LOO (folds = 736 (number of observations))
ridge_cvloo_no_interaction <- function(data, response_var = "y", k = 10) {
  set.seed(248)  # For reproducibility
  
  # Split data into folds
  folds <- sample(rep(1:k, length.out = nrow(data)))
  
  # Variables to store metrics
  rmse_train <- numeric(k)
  rmse_train_original <- numeric(k)
  rmse_test <- numeric(k)
  rmse_test_original <- numeric(k)
  r2_adj_train <- numeric(k)
  r2_adj_train_original <- numeric(k)
  r2_adj_test <- numeric(k)
  r2_adj_test_original <- numeric(k)
  coefficients_list <- list()  # Store coefficients for each fold
  
  # Perform cross-validation
  for (i in 1:k) {
    # Split into training and testing sets
    train_idx <- which(folds != i)
    test_idx <- which(folds == i)
    train_data <- data[train_idx, ]
    test_data <- data[test_idx, ]
    
    # Create design matrix with specific interactions and all other variables
    formula <- as.formula(paste(
      response_var,
      "~ .- 1"
    ))
    x_train <- model.matrix(formula, data = train_data)
    y_train <- train_data[[response_var]]
    x_test <- model.matrix(formula, data = test_data)
    y_test <- test_data[[response_var]]
    
    # Fit Ridge model with cross-validation to find optimal lambda
    cv_model <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 5)
    best_lambda <- cv_model$lambda.min
    
    # Fit final model with the best lambda
    ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)
    
    # Predictions on training and testing sets
    pred_train <- predict(ridge_model, newx = x_train)
    pred_test <- predict(ridge_model, newx = x_test)
    
    # Transform predictions and true values to the original scale
    pred_train_original <- exp(pred_train)
    pred_test_original <- exp(pred_test)
    y_train_original <- exp(y_train)
    y_test_original <- exp(y_test)
    
    # Calculate RMSE in logarithmic scale
    rmse_train[i] <- sqrt(mean((y_train - pred_train)^2))
    rmse_test[i] <- sqrt(mean((y_test - pred_test)^2))
    
    # Calculate RMSE in the original scale
    rmse_train_original[i] <- sqrt(mean((y_train_original - pred_train_original)^2))
    rmse_test_original[i] <- sqrt(mean((y_test_original - pred_test_original)^2))
    
    # Calculate adjusted R^2 in logarithmic scale
    sse_train <- sum((y_train - pred_train)^2)
    sst_train <- sum((y_train - mean(y_train))^2)
    r2_train <- 1 - (sse_train / sst_train)
    n_train <- length(y_train)
    p_train <- ncol(x_train)
    r2_adj_train[i] <- 1 - ((1 - r2_train) * (n_train - 1) / (n_train - p_train - 1))
    
    sse_test <- sum((y_test - pred_test)^2)
    sst_test <- sum((y_test - mean(y_test))^2)
    r2_test <- 1 - (sse_test / sst_test)
    n_test <- length(y_test)
    p_test <- ncol(x_test)
    r2_adj_test[i] <- 1 - ((1 - r2_test) * (n_test - 1) / (n_test - p_test - 1))
    
    # Calculate adjusted R^2 in the original scale
    sse_train_original <- sum((y_train_original - pred_train_original)^2)
    sst_train_original <- sum((y_train_original - mean(y_train_original))^2)
    r2_train_original <- 1 - (sse_train_original / sst_train_original)
    r2_adj_train_original[i] <- 1 - ((1 - r2_train_original) * (n_train - 1) / (n_train - p_train - 1))
    
    sse_test_original <- sum((y_test_original - pred_test_original)^2)
    sst_test_original <- sum((y_test_original - mean(y_test_original))^2)
    r2_test_original <- 1 - (sse_test_original / sst_test_original)
    r2_adj_test_original[i] <- 1 - ((1 - r2_test_original) * (n_test - 1) / (n_test - p_test - 1))
    
    # Store coefficients for each fold
    coefficients_list[[i]] <- coef(ridge_model)
  }
  
  # Return metrics and coefficients
  return(list(
    rmse_train = rmse_train,
    rmse_train_original = rmse_train_original,
    rmse_test = rmse_test,
    rmse_test_original = rmse_test_original,
    r2_adj_train = r2_adj_train,
    r2_adj_train_original = r2_adj_train_original,
    r2_adj_test = r2_adj_test,
    r2_adj_test_original = r2_adj_test_original,
    coefficients = coefficients_list
  ))
}

# Model Execution
k <- 736  # Number of folds, adjust as needed
ridge_results_l <- ridge_cvloo_no_interaction(data_train, response_var = "y", k = k)

# Output the results
cat("=== Ridge Regression with Optimal Lambda ===\n")
cat(sprintf("Average RMSE (Training, Logarithmic): %.3f\n", mean(ridge_results_l$rmse_train)))
cat(sprintf("Average RMSE (Training, Original): %.3f\n", mean(ridge_results_l$rmse_train_original)))
cat(sprintf("Average Adjusted R^2 (Training, Logarithmic): %.3f\n", mean(ridge_results_l$r2_adj_train)))
cat(sprintf("Average Adjusted R^2 (Training, Original): %.3f\n", mean(ridge_results_l$r2_adj_train_original)))

# Each fold
cat(sprintf("RMSE (Training, Logarithmic): %.3f\n", ridge_results_l$rmse_train))
cat(sprintf("RMSE (Training, Original): %.3f\n", ridge_results_l$rmse_train_original))
cat(sprintf("Adjusted R^2 (Training, Logarithmic): %.3f\n", ridge_results_l$r2_adj_train))
cat(sprintf("Adjusted R^2 (Training, Original): %.3f\n", ridge_results_l$r2_adj_train_original))

# Plot RMSE for training set (Original Scale) and Log scale
plot(1:k, ridge_results_l$rmse_train_original, type = "b", col = "blue", ylim = range(c(ridge_results_l$rmse_train_original, ridge_results_l$rmse_test_original)), xlab = "Fold", ylab = "RMSE (Original Scale)", main = "RMSE for Training Set")
plot(1:k, ridge_results_l$rmse_train, type = "b", col = "red", ylim = range(c(ridge_results_l$rmse_train, ridge_results_l$rmse_test)), xlab = "Fold", ylab = "RMSE (Log Scale)", main = "RMSE for Training Set")





## Cross validation for Ridge (k-fold) with interactions
### K fold (default 10 folds) or LOO (folds = 736 (number of observations))
ridge_cv_interaction1 <- function(data, response_var = "y", k = 10) {
  set.seed(248)  # For reproducibility
  
  # Split data into folds
  folds <- sample(rep(1:k, length.out = nrow(data)))
  
  # Variables to store metrics
  rmse_train <- numeric(k)
  rmse_train_original <- numeric(k)
  rmse_test <- numeric(k)
  rmse_test_original <- numeric(k)
  r2_adj_train <- numeric(k)
  r2_adj_train_original <- numeric(k)
  r2_adj_test <- numeric(k)
  r2_adj_test_original <- numeric(k)
  coefficients_list <- list()  # Store coefficients for each fold
  best_lambda_list <- numeric(k)  # Store best lambda for each fold
  
  # Perform cross-validation
  for (i in 1:k) {
    # Split into training and testing sets
    train_idx <- which(folds != i)
    test_idx <- which(folds == i)
    train_data <- data[train_idx, ]
    test_data <- data[test_idx, ]
    
    # Create design matrix with specific interactions and all other variables
    formula <- as.formula(paste(
      response_var,
      "~ . +log.sup.util:comercial - 1"
    ))
    x_train <- model.matrix(formula, data = train_data)
    y_train <- train_data[[response_var]]
    x_test <- model.matrix(formula, data = test_data)
    y_test <- test_data[[response_var]]
    
    # Fit Ridge model with cross-validation to find optimal lambda
    cv_model <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 5)
    best_lambda <- cv_model$lambda.min
    best_lambda_list[i] <- best_lambda  # Store the best lambda for this fold
    
    # Fit final model with the best lambda
    ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)
    
    # Predictions on training and testing sets
    pred_train <- predict(ridge_model, newx = x_train)
    pred_test <- predict(ridge_model, newx = x_test)
    
    # Transform predictions and true values to the original scale
    pred_train_original <- exp(pred_train)
    pred_test_original <- exp(pred_test)
    y_train_original <- exp(y_train)
    y_test_original <- exp(y_test)
    
    # Calculate RMSE in logarithmic scale
    rmse_train[i] <- sqrt(mean((y_train - pred_train)^2))
    rmse_test[i] <- sqrt(mean((y_test - pred_test)^2))
    
    # Calculate RMSE in the original scale
    rmse_train_original[i] <- sqrt(mean((y_train_original - pred_train_original)^2))
    rmse_test_original[i] <- sqrt(mean((y_test_original - pred_test_original)^2))
    
    # Calculate adjusted R^2 in logarithmic scale
    sse_train <- sum((y_train - pred_train)^2)
    sst_train <- sum((y_train - mean(y_train))^2)
    r2_train <- 1 - (sse_train / sst_train)
    n_train <- length(y_train)
    p_train <- ncol(x_train)
    r2_adj_train[i] <- 1 - ((1 - r2_train) * (n_train - 1) / (n_train - p_train - 1))
    
    sse_test <- sum((y_test - pred_test)^2)
    sst_test <- sum((y_test - mean(y_test))^2)
    r2_test <- 1 - (sse_test / sst_test)
    n_test <- length(y_test)
    p_test <- ncol(x_test)
    r2_adj_test[i] <- 1 - ((1 - r2_test) * (n_test - 1) / (n_test - p_test - 1))
    
    # Calculate adjusted R^2 in the original scale
    sse_train_original <- sum((y_train_original - pred_train_original)^2)
    sst_train_original <- sum((y_train_original - mean(y_train_original))^2)
    r2_train_original <- 1 - (sse_train_original / sst_train_original)
    r2_adj_train_original[i] <- 1 - ((1 - r2_train_original) * (n_train - 1) / (n_train - p_train - 1))
    
    sse_test_original <- sum((y_test_original - pred_test_original)^2)
    sst_test_original <- sum((y_test_original - mean(y_test_original))^2)
    r2_test_original <- 1 - (sse_test_original / sst_test_original)
    r2_adj_test_original[i] <- 1 - ((1 - r2_test_original) * (n_test - 1) / (n_test - p_test - 1))
    
    # Store coefficients for each fold
    coefficients_list[[i]] <- coef(ridge_model)
  }
  
  # Plot the results for lambda
  plot(cv_model$lambda, cv_model$cvm, type = "b", 
       xlab = "Lambda", ylab = "Cross-Validation MSE", 
       main = "Cross-Validation for Lambda M1", log = "x")
  points(best_lambda, min(cv_model$cvm), col = "red", pch = 19)
  
  # Add a label for the best lambda and move it to the right
  text(best_lambda * 4, min(cv_model$cvm) * 1.4, labels = paste("lambda.min =", round(best_lambda, 4)), 
       col = "red", cex = 0.8)  # Move the label 20% to the right
  
  legend("topright", legend = c("Best Lambda"), col = "red", pch = 19)
  
  # Return metrics and coefficients
  return(list(
    rmse_train = rmse_train,
    rmse_train_original = rmse_train_original,
    rmse_test = rmse_test,
    rmse_test_original = rmse_test_original,
    r2_adj_train = r2_adj_train,
    r2_adj_train_original = r2_adj_train_original,
    r2_adj_test = r2_adj_test,
    r2_adj_test_original = r2_adj_test_original,
    coefficients = coefficients_list,
    best_lambda = best_lambda_list 
  ))
}


# Model Execution
k <- 4  # Number of folds, adjust as needed
ridge_results1 <- ridge_cv_interaction1(data_train, response_var = "y", k = k)

# Output the results
cat("=== Ridge Regression with Optimal Lambda ===\n")
cat(sprintf("Average RMSE (Training, Logarithmic): %.3f\n", mean(ridge_results1$rmse_train)))
cat(sprintf("Average RMSE (Training, Original): %.3f\n", mean(ridge_results1$rmse_train_original)))
cat(sprintf("Average Adjusted R^2 (Training, Logarithmic): %.3f\n", mean(ridge_results1$r2_adj_train)))
cat(sprintf("Average Adjusted R^2 (Training, Original): %.3f\n", mean(ridge_results1$r2_adj_train_original)))
ridge_results1$best_lambda

# Each fold
cat(sprintf("RMSE (Training, Logarithmic): %.3f\n", ridge_results1$rmse_train))
cat(sprintf("RMSE (Training, Original): %.3f\n", ridge_results1$rmse_train_original))
cat(sprintf("Adjusted R^2 (Training, Logarithmic): %.3f\n", ridge_results1$r2_adj_train))
cat(sprintf("Adjusted R^2 (Training, Original): %.3f\n", ridge_results1$r2_adj_train_original))

# Plot RMSE for training set (Original Scale) and Log scale
plot(1:k, ridge_results1$rmse_train_original, type = "b", col = "blue", ylim = range(c(ridge_results1$rmse_train_original, ridge_results1$rmse_test_original)), xlab = "Fold", ylab = "RMSE (Original Scale)", main = "RMSE for Training Set")
plot(1:k, ridge_results1$rmse_train, type = "b", col = "red", ylim = range(c(ridge_results1$rmse_train, ridge_results1$rmse_test)), xlab = "Fold", ylab = "RMSE (Log Scale)", main = "RMSE for Training Set")

# Plot Adjusted R^2 for training set (Original Scale) and Log Scale
plot(1:k, ridge_results1$r2_adj_train_original, type = "b", col = "blue", ylim = range(c(ridge_results1$r2_adj_train_original, ridge_results1$r2_adj_test_original)), xlab = "Fold", ylab = "Adjusted R^2 (Original Scale)", main = "Adjusted R^2 for Training Set")
plot(1:k, ridge_results1$r2_adj_train, type = "b", col = "red", ylim = range(c(ridge_results1$r2_adj_train, ridge_results1$r2_adj_test)), xlab = "Fold", ylab = "Adjusted R^2 (Log Scale)", main = "Adjusted R^2 for Training Set")






ridge_cv_interaction2 <- function(data, response_var = "y", k = 10) {
  set.seed(248)  # For reproducibility
  
  # Split data into folds
  folds <- sample(rep(1:k, length.out = nrow(data)))
  
  # Variables to store metrics
  rmse_train <- numeric(k)
  rmse_train_original <- numeric(k)
  rmse_test <- numeric(k)
  rmse_test_original <- numeric(k)
  r2_adj_train <- numeric(k)
  r2_adj_train_original <- numeric(k)
  r2_adj_test <- numeric(k)
  r2_adj_test_original <- numeric(k)
  coefficients_list <- list()  # Store coefficients for each fold
  
  # Perform cross-validation
  for (i in 1:k) {
    # Split into training and testing sets
    train_idx <- which(folds != i)
    test_idx <- which(folds == i)
    train_data <- data[train_idx, ]
    test_data <- data[test_idx, ]
    
    # Create design matrix with specific interactions and all other variables
    formula <- as.formula(paste(
      response_var,
      "~ . +log.sup.util:banos + Pobl.0_14_div_Poblac.Total:tipo.casa + log.sup.util:comercial - 1"
    ))
    x_train <- model.matrix(formula, data = train_data)
    y_train <- train_data[[response_var]]
    x_test <- model.matrix(formula, data = test_data)
    y_test <- test_data[[response_var]]
    
    # Fit Ridge model with cross-validation to find optimal lambda
    cv_model <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 5)
    best_lambda <- cv_model$lambda.min
    
    # Fit final model with the best lambda
    ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)
    
    # Predictions on training and testing sets
    pred_train <- predict(ridge_model, newx = x_train)
    pred_test <- predict(ridge_model, newx = x_test)
    
    # Transform predictions and true values to the original scale
    pred_train_original <- exp(pred_train)
    pred_test_original <- exp(pred_test)
    y_train_original <- exp(y_train)
    y_test_original <- exp(y_test)
    
    # Calculate RMSE in logarithmic scale
    rmse_train[i] <- sqrt(mean((y_train - pred_train)^2))
    rmse_test[i] <- sqrt(mean((y_test - pred_test)^2))
    
    # Calculate RMSE in the original scale
    rmse_train_original[i] <- sqrt(mean((y_train_original - pred_train_original)^2))
    rmse_test_original[i] <- sqrt(mean((y_test_original - pred_test_original)^2))
    
    # Calculate adjusted R^2 in logarithmic scale
    sse_train <- sum((y_train - pred_train)^2)
    sst_train <- sum((y_train - mean(y_train))^2)
    r2_train <- 1 - (sse_train / sst_train)
    n_train <- length(y_train)
    p_train <- ncol(x_train)
    r2_adj_train[i] <- 1 - ((1 - r2_train) * (n_train - 1) / (n_train - p_train - 1))
    
    sse_test <- sum((y_test - pred_test)^2)
    sst_test <- sum((y_test - mean(y_test))^2)
    r2_test <- 1 - (sse_test / sst_test)
    n_test <- length(y_test)
    p_test <- ncol(x_test)
    r2_adj_test[i] <- 1 - ((1 - r2_test) * (n_test - 1) / (n_test - p_test - 1))
    
    # Calculate adjusted R^2 in the original scale
    sse_train_original <- sum((y_train_original - pred_train_original)^2)
    sst_train_original <- sum((y_train_original - mean(y_train_original))^2)
    r2_train_original <- 1 - (sse_train_original / sst_train_original)
    r2_adj_train_original[i] <- 1 - ((1 - r2_train_original) * (n_train - 1) / (n_train - p_train - 1))
    
    sse_test_original <- sum((y_test_original - pred_test_original)^2)
    sst_test_original <- sum((y_test_original - mean(y_test_original))^2)
    r2_test_original <- 1 - (sse_test_original / sst_test_original)
    r2_adj_test_original[i] <- 1 - ((1 - r2_test_original) * (n_test - 1) / (n_test - p_test - 1))
    
    # Store coefficients for each fold
    coefficients_list[[i]] <- coef(ridge_model)
  }
  
  # Plot the results for lambda
  plot(cv_model$lambda, cv_model$cvm, type = "b", 
       xlab = "Lambda", ylab = "Cross-Validation MSE", 
       main = "Cross-Validation for Lambda M2", log = "x")
  points(best_lambda, min(cv_model$cvm), col = "red", pch = 19)
  
  # Add a label for the best lambda and move it to the right
  text(best_lambda * 4, min(cv_model$cvm) * 1.4, labels = paste("lambda.min =", round(best_lambda, 4)), 
       col = "red", cex = 0.8)  # Move the label 20% to the right
  
  legend("topright", legend = c("Best Lambda"), col = "red", pch = 19)
  
  # Average coefficients across folds
  avg_coefficients <- apply(do.call(cbind, coefficients_list), 1, mean)
  
  # Return metrics and coefficients
  return(list(
    rmse_train = rmse_train,
    rmse_train_original = rmse_train_original,
    rmse_test = rmse_test,
    rmse_test_original = rmse_test_original,
    r2_adj_train = r2_adj_train,
    r2_adj_train_original = r2_adj_train_original,
    r2_adj_test = r2_adj_test,
    r2_adj_test_original = r2_adj_test_original,
    coefficients = coefficients_list,
    avg_coefficients = avg_coefficients,
    best_lambda = best_lambda
  ))
}

par(mfrow=c(1,2))
# Model Execution
k <- 4  # Number of folds, adjust as needed
ridge_results2 <- ridge_cv_interaction2(data_train, response_var = "y", k = k)

# Output the results
cat("=== Ridge Regression with Optimal Lambda ===\n")
cat(sprintf("Average RMSE (Training, Logarithmic): %.3f\n", mean(ridge_results2$rmse_train)))
cat(sprintf("Average RMSE (Training, Original): %.3f\n", mean(ridge_results2$rmse_train_original)))
cat(sprintf("Average Adjusted R^2 (Training, Logarithmic): %.3f\n", mean(ridge_results2$r2_adj_train)))
cat(sprintf("Average Adjusted R^2 (Training, Original): %.3f\n", mean(ridge_results2$r2_adj_train_original)))
ridge_results2$best_lambda
# Each fold
cat(sprintf("RMSE (Training, Logarithmic): %.3f\n", ridge_results2$rmse_train))
cat(sprintf("RMSE (Training, Original): %.3f\n", ridge_results2$rmse_train_original))
cat(sprintf("Adjusted R^2 (Training, Logarithmic): %.3f\n", ridge_results2$r2_adj_train))
cat(sprintf("Adjusted R^2 (Training, Original): %.3f\n", ridge_results2$r2_adj_train_original))

# Plot RMSE for training set (Original Scale) and Log scale
plot(1:k, ridge_results2$rmse_train_original, type = "b", col = "blue", ylim = range(c(ridge_results2$rmse_train_original, ridge_results2$rmse_test_original)), xlab = "Fold", ylab = "RMSE (Original Scale)", main = "RMSE for Training Set")
plot(1:k, ridge_results2$rmse_train, type = "b", col = "red", ylim = range(c(ridge_results2$rmse_train, ridge_results2$rmse_test)), xlab = "Fold", ylab = "RMSE (Log Scale)", main = "RMSE for Training Set")

# Plot Adjusted R^2 for training set (Original Scale) and Log Scale
plot(1:k, ridge_results2$r2_adj_train_original, type = "b", col = "blue", ylim = range(c(ridge_results2$r2_adj_train_original, ridge_results2$r2_adj_test_original)), xlab = "Fold", ylab = "Adjusted R^2 (Original Scale)", main = "Adjusted R^2 for Training Set")
plot(1:k, ridge_results2$r2_adj_train, type = "b", col = "red", ylim = range(c(ridge_results2$r2_adj_train, ridge_results2$r2_adj_test)), xlab = "Fold", ylab = "Adjusted R^2 (Log Scale)", main = "Adjusted R^2 for Training Set")

# Output the coefficients (avg. coefficients)
cat("=== Average Coefficients ===\n")
print(ridge_results2$avg_coefficients)


### Final Model
formula <- as.formula(paste(
  "y", 
  "~ . + log.sup.util:banos + Pobl.0_14_div_Poblac.Total:tipo.casa + log.sup.util:comercial - 1"
))

x_train <- model.matrix(formula, data = data_train)
y_train <- data_train$y

final_Ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = ridge_results2$best_lambda)

save(final_lm1_model, file = "Modelos Marcos/final_Ridge_model.RData")
load("Modelos Modelos/final_Ridge_model.RData")
