### Ridge Regression Example with Prediction
library(glmnet)
library(readxl)
library(dplyr)

### Auxiliary functions
preprocess = function(data){
  
  # Eliminate
  data$train_indices <- NULL
  
  # Eliminate 
  data$barrio <- NULL
  
  # Eliminate 
  data$cod_barrio <- NULL
  
  # Eliminate 
  data$cod_distrito <- NULL
  
  
  # Eliminate gasses
  data$M.30 <- NULL  
  data$CO <- NULL  
  data$PM10 <- NULL  
  data$NO2 <- NULL
  data$Nox <- NULL
  data$O3 <- NULL
  
  
  
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


# Read and preprocess the training and test datasets
data_train <- read_excel("Data/data_train.xlsx") %>% preprocess()
data_test <- read_excel("Data/data_test.xlsx") %>% preprocess()
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
# write.csv(predicted_values, "predicted_prices.csv", row.names = FALSE)


k_fold_cv_ridge <- function(data_train, k = 4, alpha = 0) {
  cat("=== Running k_fold Cross Validation --- Ridge === \n")
  
  # Create the K-fold partition
  folded_data <- k_fold(data_train, k)$.folds
  
  # Initialize vectors to store metrics
  cv_rmse_y <- numeric(k)
  cv_rmse_logy <- numeric(k)
  cv_rsq_adj_logy <- numeric(k)
  
  X = model.matrix(y ~ . - 1, data = data_train)
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
results <- k_fold_cv_ridge(data_train = data_ridge)
print(results)


