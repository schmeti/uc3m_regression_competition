# Necessary Libraries ---------------------------------------------------------
if (!require(readxl)) install.packages("readxl")
if (!require(leaps)) install.packages("leaps")
if (!require(caret)) install.packages("caret")
if (!require(groupdata2)) install.packages("groupdata2")
if (!require(car)) install.packages("car")
if (!require(dplyr)) install.packages("dplyr")
if (!require(Metrics)) install.packages("Metrics")
if (!require(openxlsx)) install.packages("openxlsx")



library(readxl)
library(leaps)
library(caret)
library(groupdata2)
library(car)
library(dplyr)
library(Metrics)
library(openxlsx)



# Initial Transformations of the data ------------------------------------------
if(!require(geosphere)) install.packages("geosphere")
library(geosphere)

# Multicollinearity Analysis
if(!require(car)) install.packages("car")
library(car)

multicollinearity_analysis <- function(data){
  
  # Obtain the numeric 'section' of the data matrix
  numeric_column_names <- names(data)[sapply(data, is.numeric)]
  X <- as.matrix(data[, numeric_column_names])

  # Condition number
  lambda <- eigen(t(X)%*%X)$values # Eigenvalues
  
  if (min(lambda) <= 10^{-5}){
    cn <- Inf
  } else{
    cn <- sqrt(max(lambda)/min(lambda))
  }
  
  # VIF 
  model <- lm(log.precio.house.m2 ~ ., data = as.data.frame(X))
  vif <- vif(model)
  
  # Return a list with the parameters
  return(list(CN = cn, VIF = vif))
}
#-------------------------------------------------------------------------------


### Helper Functions

## Test/Train Split
train_test_split <- function(data){
  train_index <- createDataPartition(data$precio.house.m2, p=0.7, list = FALSE)
  data_train <- data[train_index, ]
  data_test <- data[-train_index, ]
  return(list(data_train = data_train, data_test = data_test))
}

## K-Fold for Cross-Validation -----------------------------------------------
k_fold <- function(data,k) {
  
  # Identify numerical and categorical variables
  num_id <- sapply(data, is.numeric)
  num_vars <- names(data)[num_id]
  cat_vars <- names(data)[!num_id]
  
  # Create a k-fold partition with balanced cat_vars and which
  # tries to minimize similar values in "precio.house.m2"
  print(k)
  folded_data <- fold(data, 
                      k = k, 
                      cat_col = cat_vars,#if (length(cat_vars) > 0) cat_vars else NULL, # if no categoric variables, then null 
                      num_col = "precio.house.m2")
  
  print(folded_data)
  ### OR log.precio.house.m2
  
  # It adds a new variable, .folds, which assigns a value 1 to k to each
  # instance, dividing them by folds
  
  # Return the new dataset
  return(folded_data)
}


## K-fold cross-validation ------------------------------------------------------
k_fold_cv_linear_model <- function(model_formula, 
                                   data_train,
                                   k=2){
  cat("=== running k_fold Cross Validation ===")
  
  # Create the K-fold partition
  folded_data <- k_fold(data_train,k)$.folds

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
    
    print(temp_train)
    
    # Fit the model and make predictions
    temp_model <- fit_linear_model(model_formula, temp_train)
    temp_predictions <- predict(temp_model, newdata = temp_test)
    
    ## Calculate error metrics and store them
    
    # rmse
    cv_rmse[i] <- sqrt(mean((temp_predictions - temp_test$precio.house.m2)^2))
    
    # rsq adj
    n_train = nrow(temp_train)
    num_predictors = ncol(temp_train)-1
    print(num_predictors)
    
    
    SSE = sum((temp_test$precio.house.m2 - temp_predictions )^2)
    SSR = sum((mean(temp_test$precio.house.m2) - temp_predictions)^2)
    SST = SSE + SSE

    cv_rsq_adj[i] = 1 - (((SSE/(SSE+SSR))*(n_train-1))/(n_train-num_predictors-1))
  }
  
  # Return the vector with rmse for each k-fold
  return(list(cv_rmse=cv_rmse,
              cv_rsq_adj=cv_rsq_adj))
}

## Leave one out - cross-validation ------------------------------------------------------
loo_cv_linear_model <- function(model_formula, 
                                   data_train){
  cat("=== running Leave one Out Cross Validation ===\n")
  n <- nrow(data_train)
  cv_rmse <- numeric(n)
  cv_rsq_adj <- numeric(n)
  

  for (i in 1:n){
    # split
    temp_train <- data_train[-i, ]
    temp_test <- data_train[i, , drop = FALSE]
    
    # Fit the model and make predictions
    temp_model <- fit_linear_model(model_formula, temp_train)
    temp_predictions <- exp(predict(temp_model, newdata = temp_test))
    
    y = exp(temp_test$y)
    
    # rmse
    cv_rmse[i] <- sqrt(mean((temp_predictions - y)^2))
    
    # rsq adj
    n_train = nrow(temp_train)
    num_predictors = ncol(temp_train)-1
    
    SSE = sum((y - temp_predictions )^2)
    SSR = sum((mean(y) - temp_predictions)^2)
    SST = SSE + SSE
    
    cv_rsq_adj[i] = 1 - (((SSE/(SSE+SSR))*(n_train-1))/(n_train-num_predictors-1))
  }
  
  # Return the vector with rmse for each k-fold
  return(list(cv_rmse=cv_rmse,
              cv_rsq_adj=cv_rsq_adj))
}


## Check Multicoliniearity -----------------------------------------------------
check_multicollinearity <- function(model, data) {
  
  # Identify numerical and categorical variables
  predictors <- labels(terms(model)) # Variables used in the model
  predictors <- predictors[!grepl(":", predictors)] # Exclude interaction terms
  data <- data[,c("y", predictors)]
  num_id <- sapply(data, is.numeric)
  num_vars <- setdiff(names(data)[num_id], "y")
  cat_vars <- names(data)[!num_id]
  
  new_model_formula <- as.formula(
    paste("y ~", paste(predictors, collapse = " + "))
  )
  new_model = lm(new_model_formula,data = data)
  
  # Model Matrix
  X <- data[,num_vars]
  R <- cor(X)
  
  # Calculate condition number using kappa
  condition_number <- kappa(R, exact = TRUE)
  
  # Calculating VIF values using the car package
  vif_values <- tryCatch({
    vif(new_model)
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
  
  # Eliminate
  data$train_indices <- NULL
  
  # Eliminate 
  data$barrio <- NULL
  
  # Eliminate 
  data$cod_barrio <- NULL
  
  # Eliminate 
  data$cod_distrito <- NULL
  
  # Logarithmic objective variable
  if ("precio.house.m2" %in% colnames(data)) {
    data$y <- log(data$precio.house.m2) 
    data$precio.house.m2 <- NULL # Eliminate the old variable
  }
  
  

  # Eliminate sup.const for collinearity reasons
  data$sup.const <- NULL

  # Logarithmic transform
  data$log.sup.util <- log(data$sup.util) 
  data$sup.util <- NULL
  
  # Eliminate casco.historico for duplcate information reasons
  data$casco.historico <- NULL
  
  # Eliminate M.30 for duplcate information reasons
  data$M.30 <- NULL
  
  # radius
  # Load required library
  library(geosphere)
  # Central point (Puerta del Sol)
  center <- c(-3.7038, 40.4168)
    
  # Calculate distances and add a new column
  data$radius <- distHaversine(
    matrix(c(data$longitud, data$latitud), ncol = 2),
    matrix(rep(center, nrow(data)), ncol = 2, byrow = TRUE)
  ) / 1000  # Convert meters to kilometers
  
  # Turn categorical columns to factors
  factor_columns <- c("distrito", "banos", "dorm", "tipo.casa", "inter.exter", 
                      "ascensor", "estado", "comercial")
  data[factor_columns] <- lapply(data[factor_columns], as.factor)
  
  # group categories via manual evaluation
  # Dorm
  data <- data %>%
    mutate(
      dorm = case_when(
        dorm %in% c("0", "1") ~ "0-1",
        dorm %in% c("4", "5", "6", "7") ~ "+4",
        TRUE ~ as.character(dorm)  
      )
    )
  data$dorm <- factor(data$dorm, levels = unique(data$dorm))
  
  # Banos
  data <- data %>%
    mutate(
      banos = case_when(
        banos %in% c("3", "4", "5", "6", "7") ~ "+3",
        TRUE ~ as.character(banos)  
      )
    )
  data$banos <- factor(data$banos, levels = unique(data$banos))
  
  # tipo.casa
  data <- data %>%
    mutate(
      tipo.casa = case_when(
        tipo.casa %in% c("atico", "estudio") ~ "Atico/Estudio",
        tipo.casa %in% c("piso", "Otros") ~ "Piso",
        tipo.casa %in% c("chalet", "duplex") ~ "Chalet/Duplex",
        TRUE ~ as.character(tipo.casa)  
      )
    )
  data$tipo.casa <- factor(data$tipo.casa, levels = unique(data$tipo.casa))
  
  # estado
  data <- data %>%
    mutate(
      estado = case_when(
        estado %in% c("a_reformar", "reg,-mal") ~ "Bajo",
        estado %in% c("excelente", "nuevo-semin,", "reformado") ~ "Alto",
        estado %in% c("buen_estado", "segunda_mano") ~ "Medio",
      )
    )
  data$estado <- factor(data$estado, levels = unique(data$estado))
  

  data <- data %>%
    mutate(
      distrito = case_when(
        # South Districts
        distrito %in% c("arganzuela" ,"centro", "chamartin", "chamberi", "moncloa", "retiro", "salamanca") ~ "Center",

        # Central Districts
        distrito %in% c("carabanchel", "latina") ~ "South West",

        # North Districts
        distrito %in% c("moratalaz", "puente_vallecas", "usera", "vallecas", "vicalvaro", "villaverde") ~ "South East",

        # West Districts
        distrito %in% c("barajas","ciudad_lineal", "fuencarral", "hortaleza", "san_blas","tetuan") ~ "North East",

        # Default to original values if no match
        TRUE ~ as.character(distrito)
      )
    )
  data$distrito <- factor(data$distrito, levels = unique(data$distrito))

  # Unifying the numeric gases variables into a single dichotomic 'polluted'
  # which is to be SO2 (PM10 behaves strangely)
  pollutants <- c("CO", "NO2", "Nox", "O3", "PM10")
  for (var in pollutants){
    data[[var]] <- NULL                     
  }
  
  
  # Function to normalize multiple variables
  normalize_variables <- function(variables) {
    for (var in variables) {
      data[[var]] <- (data[[var]] - mean(data[[var]], na.rm = TRUE)) / sd(data[[var]], na.rm = TRUE)
    }
    return(data)
  }
  
  # Generate the formula automatically
  num_id <- sapply(data, is.numeric) 
  num_vars <- names(data)[num_id] %>% setdiff(c("y", "radius"))
  
  data = normalize_variables(num_vars)
  
  return(data)
}

# Fit Model   ------------------------------------------------------------------

## linear model

fit_linear_model = function(formula, data_train){
  model = lm(formula,data = data_train)
  return(model)
}

## Fit P-Splines + categorical (GAM) Model
library(mgcv)
fit_ps_model = function(data_train){
  # Generate the formula automatically
  num_id <- sapply(data_train, is.numeric)
  num_vars <- names(data_train)[num_id]
  cat_vars <- names(data_train)[!num_id]
  # Exclude the response variable
  predictors <- setdiff(num_vars, "y")  
  
  # Create the formula with p-splines for numerical vars. and straight categorical vars.
  gam_formula <- as.formula(
    paste("y ~", paste(c(paste0("s(", predictors, ", bs='ps', m = 3)"),cat_vars), collapse = " + "))
  )
  # Fit the GAM 
  gam_model <- gam(gam_formula, data = data_train)
  
  # Return the fitted model
  return(gam_model)
}

# load model
load_model = function(load_model_path){
  temp_env <- new.env()
  load(load_model_path, envir = temp_env)
  obj_name <- ls(temp_env)
  model <- temp_env[[obj_name[1]]]
  return(model)
}

# Score   ----------------------------------------------------------
score_model = function(model,
                       model_formula,
                       data,
                       print_bool = T
                       ){
  
  
  # score via k_fold
  #cv_scores = k_fold_cv_linear_model(model_formula, data)
  cv_scores = loo_cv_linear_model(model_formula, data)
  
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
  residuals <- data_test$y - predictions
  
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



# predict using model and write to excel ---------------------------------------
predict_and_write = function(data_test, model, path = "Data/predicted_prices.xlsx"){
  
  # predict
  prediction = predict(model, data_test[order(data_test$test_indices), ])
  
  # write predictions to excel
  wb <- loadWorkbook(path)
  writeData(wb, sheet = "Hoja1", x = prediction, startCol = 2, startRow = 2)
  saveWorkbook(wb, path, overwrite = TRUE)
}


################################################################################

### Run Pipeline
run_pipeline = function(data_train,
                        data_test,
                        model_formular="",
                        store_model = FALSE,
                        store_model_name = "linear_model",
                        load_model_path = "",
                        predict_and_write_path = ""){
  
  
  # preprocess data
  data_train_processed = preprocess(data = data_train)
  cat("Preprocessing -- DONE\n")
  
  
  # fit/load model
  load_model_path <- trimws(load_model_path)
  if (!is.null(load_model_path) && load_model_path != "") {
    model = load_model(load_model_path)
    model_formular = formula(model)
    cat(paste(" Load Model ",load_model_path, " -- DONE\n"))
  } else {
    model = fit_linear_model(formula = model_formular, data_train = data_train_processed)
    cat("Fit Model -- DONE\n")
    
  }
  print(summary(model))
  
  # check multicolinearity
  #multicollinearity <- check_multicollinearity(model, data_train_processed)
  #cat("Check Multicolinearity -- DONE\n")
  
  
  # predict and score on test data set
  score = score_model(model = model,
                      data = data_train_processed,
                      model_formula=model_formular)
  cat("Scoring -- DONE\n")
  
  # diagnostics plots
  diagnostic_plots(model)
  cat("Plot -- DONE\n")
  
  # predict
  if (!is.null(predict_and_write_path) && predict_and_write_path != "") {
    data_test_processed = preprocess(data = data_test)
    
    predict_and_write(data_test_processed, model,path=predict_and_write_path)
    cat("Writing Predictions to excel -- DONE\n")
  }
  
  
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

################################################################################
### run
data_train <- read_excel("Data/data_train.xlsx")
#data_test <- read_excel("Data/data_test_tryout.xlsx")

run_pipeline(data_train=data_train,
             data_test=data_test,
             #load_model_path="Modelos Nico 3/total_lm_BIC.RData",
             store_model = FALSE,
             #predict_and_write_path = "Data/predicted_prices.xlsx",
             model_formula = y ~ distrito,
             )




