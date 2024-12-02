# Necessary Libraries ---------------------------------------------------------
if (!require(readxl)) install.packages("readxl")
if (!require(leaps)) install.packages("leaps")
if (!require(caret)) install.packages("caret")
if (!require(groupdata2)) install.packages("groupdata2")
if (!require(car)) install.packages("car")
if (!require(dplyr)) install.packages("dplyr")

library(readxl)
library(leaps)
library(caret)
library(groupdata2)
library(car)
library(dplyr)


### Helper Functions

## Test/Train Split
train_test_split <- function(data){
  train_index <- createDataPartition(data$precio.house.m2, p=0.7, list = FALSE)
  data_train <- data[train_index, ]
  data_test <- data[-train_index, ]
  return(list(data_train = data_train, data_test = data_test))
}

## K-Fold for Cross-Validation -----------------------------------------------
k_fold <- function(data) {
  
  
  # Identify numerical and categorical variables
  num_id <- sapply(data, is.numeric)
  num_vars <- names(data)[num_id]
  cat_vars <- names(data)[!num_id]
  
  
  # Create a k-fold partition with balanced cat_vars and which
  # tries to minimize similar values in "precio.house.m2"
  folded_data <- fold(data, k = 2, cat_col = cat_vars, num_col = "precio.house.m2")
  # It adds a new variable, .folds, which assigns a value 1 to k to each
  # instance, dividing them by folds
  
  # Return the new dataset
  return(folded_data)
}


## K-fold cross-validation ------------------------------------------------------
k_fold_cv_linear_model <- function(model_formula, data_train){
   
  # Create the K-fold partition
  folded_data <- k_fold(data_train)$.fold
  
  # Initialize a vector to store each fold's rsme
  cv_rmse <- numeric(k)
  
  # Initialize a vector to store each fold's Rsq_adj
  cv_rsq_adj <- numeric(k)
  for (i in 1:k){
    train_id <- (1:k)[-i] # Train folds
    test_id <- i          # Test folds
    
    # Create the fold's test/train split
    temp_train <- data_train[which(folded_data!=i),]
    temp_test <- data_train[which(folded_data==i),]
    
    # Fit the model and make predictions
    temp_model <- fit_model(model_formula, temp_train)
    temp_predictions <- predict(temp_model, newdata = temp_test)
    
    ## Calculate error metrics and store them
    
    # rmse
    cv_rmse[i] <- sqrt(mean((temp_predictions - temp_test$precio.house.m2)^2))
    
    # rsq adj
    
    SSE = sum((temp_test$precio.house.m2 - temp_predictions )^2)
    SSR = sum((mean(temp_test$precio.house.m2) - temp_predictions)^2)
    SST = SSE + SSE

    cv_rsq_adj[i] = 1 - (((SSE/(SSE+SSR))*(n_train-1))/(n_train-num_predictors-1))
    
  }
  
  # Return the vector with rmse for each k-fold
  return(list(cv_rmse=cv_rmse,
              cv_rsq_adj=cv_rsq_adj))
}



## Check Multicoliniearity -----------------------------------------------------
check_multicollinearity <- function(model) {
  # Model Matrix
  X <- model.matrix(model)
  
  # Calculate condition number using kappa
  condition_number <- kappa(X, exact = TRUE)
  
  # Calculating VIF values using the car package
  vif_values <- tryCatch({
    vif(model)
  }, error = function(e) {
    warning("Could not calculate VIF due to collinearity issues.")
    return(NA)
  })
  
  # Generate warnings if there are significant problems
  if (condition_number > 30) {
    warning("High condition number detected, indicating potential multicollinearity issues.")
  }
  if (any(vif_values > 10, na.rm = TRUE)) {
    warning("VIF values greater than 10 detected, indicating potential multicollinearity issues.")
  }
  
  # Give out diagnostics 
  cat("=== Multicollinearity Diagnostics ===\n")
  if (is.na(condition_number)) {
    cat("Serious issues detected in the condition number.\n")
  } else {
    cat("Condition Number:", round(condition_number, 2), "\n")
  }
  
  cat("VIF Values:\n")
  if (all(is.na(vif_values))) {
    cat("VIF values could not be calculated due to collinearity.\n")
  } else {
    print(vif_values)
  }
  
  # Return results
  return(list(
    condition_number = condition_number,
    vif_values = vif_values
  ))
}

## 5. Diagnostic Plots ---------------------------------------------------------
diagnostic_plots <- function(model) {
  par(mfrow = c(2, 2)) # Configure layout for 4 plots
  
  # Suppress warnings for leverage points and create the plots
  suppressWarnings({
    # Plot 1: Residuals vs Fitted
    plot(model, which = 1)    # Linearity and heteroscedasticity
    
    # Plot 2: Normal Q-Q
    plot(model, which = 2)            # Normality of residuals
    
    # Plot 3: Scale-Location
    plot(model, which = 3)        # Heteroscedasticity
    
    # Plot 4: Residuals vs Leverage (Filtering high leverage points)
    leverage_values <- hatvalues(model)
    high_leverage <- which(leverage_values > 0.99)  # Puntos con apalancamiento alto
    plot(model, which = 5, subset = -high_leverage) # Filtrado de puntos con alta influencia
  })
  
  par(mfrow = c(1, 1)) # Reset layout
}

### Pipeline Components

## Preprocess  -----------------------------------------------------------------
preprocess = function(data){
  # Transformar la variable objetivo
  data$precio.house.m2 <- log(data$precio.house.m2)
  
  # Definir columnas categóricas
  factor_columns <- c("barrio", "distrito", "tipo.casa", "inter.exter", 
                      "ascensor", "estado", "comercial", "casco.historico", "M.30")
  
  # Convertir a factores
  data[factor_columns] <- lapply(data[factor_columns], as.factor)
  
  # Eliminar columnas no deseadas
  data <- data %>% select(-c(cod_barrio, cod_distrito, train_indices))
  return(data)
}

# Fit Model   ------------------------------------------------------------------
fit_linear_model = function(formula, data_train){
  model = lm(formula,data = data_train)
  return(model)
}

# Fit P-Splines + categorical (GAM) Model --------------------------------------
fit_ps_model = function(data_train){
  # Generate the formula automatically
  num_id <- sapply(data_train, is.numeric)
  num_vars <- names(data_train)[num_id]
  cat_vars <- names(data_train)[!num_id]
  # Exclude the response variable
  predictors <- setdiff(num_vars, "y")  
  
  # Create the formula with p-splines for numerical vars. and striaght categorical vars.
  gam_formula <- as.formula(
    paste("y ~", paste(c(paste0("s(", predictors, ", bs='ps', k = 40, m = 3)"),cat_vars), collapse = " + "))
  )
  # Fit the GAM 
  gam_model <- gam(gam_formula, data = data_train)
  
  # Return the fitted model
  return(gam_model)
}


# Predict and score   ----------------------------------------------------------
predict_and_score = function(model,
                             model_formula,
                             data_train,
                             print_bool = T
                             ) {
  
  
  # score via k_fold
  cv_scores = k_fold_cv_linear_model(model_formula, data_train)
  
  # print scores
  if(print_bool){
    cat("=== Model Performance ===\n")
    cat(paste0("Mean RMSE: ", mean(cv_scores$cv_rmse), "\n"))
    cat(paste0("Mean R^2_adj: ", mean(cv_scores$cv_rsq_adj), "\n"))
    cat(paste0("from ",length(cv_scores$cv_rmse)," fold cross validation \n"))
  }

  return(cv_scores)
}


plot_res <- function(model, data_test) {
  # Calculate fitted values and residuals
  predictions <- predict(model, newdata = data_test)
  residuals <- data_test$precio.house.m2 - predictions
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Fitted = predictions,
    Residuals = residuals
  )
  
  # Generate the plot
  plot <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(
      title = "Fitted Values vs Residuals",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal()
  
  # Print the plot to display it
  print(plot)
}



### Run Pipeline
run_pipeline = function(data,
                        model_formular,
                        store_model = FALSE,
                        store_model_name = "linear_model",
                        load_model_path = ""){
  
  # Train,test split
  split_data <- train_test_split(data = data)
  data_train <- split_data$data_train
  data_test <- split_data$data_train
  cat("Train,Test Split -- DONE\n")
  

  # preprocess
  data_processed = preprocess(data = data_train)
  cat("Preprocessing -- DONE\n")
  
  # fit/load model
  load_model_path <- trimws(load_model_path)
  if (!is.null(load_model_path) && load_model_path != "") {
    model = readRDS(load_model_path)
    print(paste(" === Model: ",load_model_path, " loaded successfully ===\n"))
  } else {
    cat("=== Fitting a new model ===\n")
    model = fit_linear_model(formula = model_formular, data_train = data_processed)
  }
  cat("Load/Fit Model -- DONE\n")
  
  
  # check multicolinearity
  multicollinearity <- check_multicollinearity(model)
  cat("Check Multicolinearity -- DONE\n")
  
  
  # predict and score on test data set
  score = predict_and_score(model = model,
                            data_train = data_train,
                            model_formula=model_formular)
  cat("Score -- DONE\n")
  
  # diagnostics plots
  diagnostic_plots(model)
  cat("Plot -- DONE\n")
  
  
  
  # save model
  if(store_model){
    # Save model with a properly constructed path using file.path()
    #model_file_path <- file.path("models", paste0(store_model_name, "_", gsub("[:/ ]", "_", Sys.time()), ".rds"))
    current_time <- Sys.time()
    model_file_path <- file.path("models", paste0(store_model_name, "_",gsub(" ","_",as.POSIXct(format(current_time, "%Y-%m-%d %H-%M"))),".rds"))
    saveRDS(model, file = model_file_path)
    print(paste("Model saved to", model_file_path))
    cat(paste0("Save model to", model_file_path,"-- DONE\n"))
  }
}

# execute
data <- read_excel("Data/data_train.xlsx")
run_pipeline(data,
             model_formula = precio.house.m2 ~ . - barrio - distrito,
             store_model = F,
             #load_model_path = "models/linear_model_2024-11-25.rds"
             )

