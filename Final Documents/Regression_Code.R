library(readxl)
library(leaps)
library(caret)
library(groupdata2)
library(car)
library(dplyr)
library(mgcv)
library(MASS)
library(geosphere)

### Preprocess & Data ----------------------------------------------------------

data <- read_excel("Data/data_train.xlsx")

preprocess = function(data,
                      predictors){
  
  # Eliminate
  data$train_indices <- NULL
  
  # Eliminate 
  data$barrio <- NULL
  
  # Eliminate 
  data$cod_barrio <- NULL
  
  # Eliminate 
  data$cod_distrito <- NULL
  
  # Logarithmic objective variable
  data$y <- log(data$precio.house.m2) 
  data$precio.house.m2 <- NULL # Eliminate the old variable
  
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
        banos %in% c("3", "4", "5", "7") ~ "+3",
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
  
  # Eliminar columnas no deseadas
  data <- subset(data, select=predictors)
  
  return(data)
}

# Preprocess the data
data_train <- preprocess(data)

### K-fold CV ------------------------------------------------------------------
k_fold <- function(data, k, cat_vars = c("tipo.casa"), obj_var = "y") {
  # Create a k-fold partition with balanced cat_vars and which
  # tries to minimize similar values in obj_var
  folded_data <- fold(data, 
                      k = k, 
                      cat_col = cat_vars,
                      num_col = obj_var)
  
  # It adds a new variable, .folds, which assigns a value 1 to k to each
  # instance, dividing them by folds
  
  # Return the new dataset
  return(folded_data)
}

### Loop to find a comprehensively balanced seed for the k-fold
# seed <- 1
# mix <- 1000000
# k = 4
# Tot_table <- list()
# n <- 736
# 
# num_id <- sapply(data_train, is.numeric)
# num_vars <- setdiff(names(data_train)[num_id], "y")
# cat_vars <- names(data_train)[!num_id]
# 
# for (var in cat_vars){
#   Tot_table[[as.name(var)]] = table(data_train[[as.name(var)]])/n
# }
# for (i in 1:1000){
#   set.seed(i)
#   folded_data <- fold(data_train,
#                       k = k,
#                       cat_col = "tipo.casa",
#                       num_col = "y")
#   mix_aux <- 0
#   for (j in 1:k){
#     temp_indexes <- which(folded_data$.folds == j)
#     ll <- length(temp_indexes)
#     for(var in cat_vars){
#       mix_aux= mix_aux + sum(abs(table(data_train[[as.name(var)]][which(folded_data$.folds == j)])/ll - Tot_table[[as.name(var)]]))
#     }
#   }
#   print(i)
#   print(mix_aux)
#   if (mix_aux < mix){
#     seed <- i
#     mix <- mix_aux
#   }
# }
# print(seed) # Up until 2000 ---> 248 is the best (k=4)

k_fold <- function(data, k=4, cat_vars = c("tipo.casa"), obj_var = "y") {
  # Set the previously studied best seed (balance-wise)
  set.seed(248)
  
  # Create a k-fold partition with balanced cat_vars and which
  # tries to minimize similar values in obj_var
  folded_data <- fold(data, 
                      k = k, 
                      cat_col = cat_vars,
                      num_col = obj_var)
  
  # It adds a new variable, .folds, which assigns a value 1 to k to each
  # instance, dividing them by folds
  
  # Return the new dataset
  return(folded_data)
}

folded_data <- k_fold(data_train)

# for (j in 1:k){
#   print(paste(j, "- Fold   ==================================================="))
#   for(var in cat_vars){
#     print(paste(var,"----------------------------------------------------------"))
#     print(table(data_train[[as.name(var)]][which(folded_data$.folds == j)])/ll)  
#     print(Tot_table[[as.name(var)]])
#   }
# }


### Linear Models CV -----------------------------------------------------------
fit_linear_model = function(formula, data_train){
  model = lm(formula,data = data_train)
  return(model)
}

k_fold_cv_linear_model <- function(model_formula, 
                                   data_train,
                                   k=4){
  cat("=== Running k_fold Cross Validation --- lm === \n")
  
  # Create the K-fold partition
  folded_data <- k_fold(data_train,k)$.folds
  
  # Initialize a vector to store each fold's rsme
  cv_rmse_y <- numeric(k)
  
  # Initialize a vector to store each fold's rsme
  cv_rmse_logy <- numeric(k)
  
  # Initialize a vector to store each fold's Rsq_adj
  cv_rsq_adj_logy <- numeric(k)
  
  for (i in 1:k){
    # Create the fold's test/train split
    temp_train <- data_train[which(folded_data!=i),]
    temp_test <- data_train[which(folded_data==i),]
    
    # Fit the model and make predictions
    temp_model <- fit_linear_model(model_formula, temp_train)
    temp_predictions <- predict(temp_model, newdata = temp_test)
    
    ## Calculate error metrics and store them
    
    # RsqAdj in log(y)
    n_test = nrow(temp_test)
    num_predictors = length(coefficients(temp_model)) 
    
    SSE = sum((temp_test$y - temp_predictions)^2)
    SST = sum((mean(temp_test$y) - temp_test$y)^2)
    
    cv_rsq_adj_logy[i] = 1 - (SSE/(n_test-num_predictors))/(SST/(n_test-1))
    
    # RMSE in log(y)
    cv_rmse_logy[i] <- sqrt(SSE/n_test)
    
    # RMSE in y
    SSE_exp = sum((exp(temp_test$y) - exp(temp_predictions))^2)
    cv_rmse_y[i] <- sqrt(SSE_exp/n_test)
  }
  
  # Return the vector with rmse for each k-fold
  return(list(cv_rmse_y=cv_rmse_y,
              mean_cv_rmse_y = mean(cv_rmse_y),
              cv_rmse_logy=cv_rmse_logy,
              mean_cv_rmse_logy = mean(cv_rmse_logy),
              cv_rsq_adj_logy=cv_rsq_adj_logy,
              mean_cv_rsq_adj_logy = mean(cv_rsq_adj_logy)
  ))
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
  return()
}

########################## Linear Models #######################################

#### Modelling: No interactions ================================================
## Base
num_id <- sapply(data_train, is.numeric)
num_vars <- setdiff(names(data_train)[num_id], c("y"))
num_vars
cat_vars <- names(data_train)[!num_id]
cat_vars

lm_formula <- as.formula(
  paste("y ~", paste(c(num_vars, cat_vars), collapse = " + "))
)
lm_model = lm(lm_formula,data = data_train)
summary(lm_model)
k_fold_cv_linear_model(lm_formula, data_train)
check_multicollinearity(lm_model, data_train)

## Step BIC
n <- 736
lm_BIC <- stepAIC(lm_model, direction = 'both', k = log(n))
summary(lm_BIC)
BIC_predictors <- labels(terms(lm_BIC))
lm_BIC_formula <- as.formula(
  paste("y ~", paste(BIC_predictors, collapse = " + "))
)
k_fold_cv_linear_model(lm_BIC_formula, data_train)
check_multicollinearity(lm_BIC, data_train)
# Diagnostics
par(mfrow = c(2, 2))
plot(lm_BIC)

## Step AIC
lm_AIC <- stepAIC(lm_model, direction = 'both')
summary(lm_AIC)
AIC_predictors <- labels(terms(lm_AIC))
lm_AIC_formula <- as.formula(
  paste("y ~", paste(AIC_predictors, collapse = " + "))
)
k_fold_cv_linear_model(lm_AIC_formula, data_train)
check_multicollinearity(lm_AIC, data_train)
# Diagnostics
par(mfrow = c(2, 2))
plot(lm_AIC)

anova(lm_AIC)

### Modeling: Interactions =====================================================

## Directly from BIC -----------------------------------------------------------
BIC_predictors <- labels(terms(lm_BIC))
num_BIC_predictors <- c(BIC_predictors[1:5], "log.sup.util") # Added log.sup.util bc we consider it to be important
cat_BIC_predictors <- c(BIC_predictors[6:10], "tipo.casa") # Added tipo.casa bc we consider it to be important
combinations1 <- expand.grid(num_BIC_predictors, cat_BIC_predictors)
inter_new1 <- paste(combinations1$Var1, combinations1$Var2, sep = ":")
# Create plots for each interaction
plots <- lapply(inter_new1, function(interaction) {
  # Split the interaction into individual variables
  vars <- unlist(strsplit(interaction, ":"))
  
  ggplot(data_train, aes(x = !!as.name(vars[1]), y = y, color = !!as.name(vars[2]))) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = paste("Interaction:", interaction),
         x = vars[1], y = "y") +
    theme_minimal()
})
m=1
plots[[m]];m=m+1

inter_selected <- c("log.sup.util:comercial", "radius:tipo.casa", 
                    "latitud:tipo.casa", "log.sup.util:ascensor", 
                    "ref.hip.zona:banos", "log.sup.util:banos")
# Create plots for each interaction
plots <- lapply(inter_selected, function(interaction) {
  # Split the interaction into individual variables
  vars <- unlist(strsplit(interaction, ":"))
  
  ggplot(data_train, aes(x = !!as.name(vars[1]), y = y, color = !!as.name(vars[2]))) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = paste("Interaction:", interaction),
         x = vars[1], y = "y") +
    theme_minimal()
})
m=1
plots[[m]];m=m+1

inter_selected <- c("log.sup.util:comercial")

added_BIC_predictors <- c(num_BIC_predictors, cat_BIC_predictors, inter_selected)
added_BIC_formula <- as.formula(
  paste("y ~", paste(added_BIC_predictors, collapse = " + "))
)
added_BIC_model = lm(added_BIC_formula, data_train)
summary(added_BIC_model)
anova(added_BIC_model)
k_fold_cv_linear_model(added_BIC_formula, data_train)
check_multicollinearity(added_BIC_model, data_train)


## Directly from AIC -----------------------------------------------------------
AIC_predictors <- labels(terms(lm_AIC))
num_AIC_predictors <- AIC_predictors[1:9]
cat_AIC_predictors <- AIC_predictors[10:16]
combinations1 <- expand.grid(num_AIC_predictors, cat_AIC_predictors)
inter_new1 <- paste(combinations1$Var1, combinations1$Var2, sep = ":")


added_AIC_predictors <- c(num_AIC_predictors, cat_AIC_predictors, inter_new1)
added_AIC_formula <- as.formula(
  paste("y ~", paste(added_AIC_predictors, collapse = " + "))
)
added_AIC_model = lm(added_AIC_formula, data_train)
summary(added_AIC_model)
anova(added_AIC_model)
k_fold_cv_linear_model(added_AIC_formula, data_train)
check_multicollinearity(added_AIC_model, data_train)

## Apply BIC to the new model to make it sparser
sparse_AIC_model = stepAIC(added_AIC_model, direction = 'both', k = log(736))
summary(sparse_AIC_model)
k_fold_cv_linear_model(sparse_AIC_model, data_train)


# Call the final lm models =====================================================
## Model 1 --- Manual selection after BIC --------------------------------------
final_lm1_predictors <- added_BIC_predictors
final_lm1_formula <- as.formula(
  paste("y ~", paste(final_lm1_predictors, collapse = " + "))
)
final_lm1_model = lm(final_lm1_formula, data_train)
save(final_lm1_model, file = "Modelos Nico/final_lm1_model.RData")
load("Modelos Nico/final_lm1_model.RData")

final_lm1_predictors <- labels(terms(final_lm1_model))

summary(final_lm1_model)
anova(final_lm1_model)
k_fold_cv_linear_model(final_lm1_model, data_train)
check_multicollinearity(final_lm1_model, data_train)
# Diagnostics
par(mfrow = c(2, 2))
plot(final_lm1_model)

## Model 2 --- Sparse AIC after interacting ------------------------------------
final_lm2_predictors <- c(labels(terms(sparse_AIC_model))[1:11], c("dorm"), 
                          labels(terms(sparse_AIC_model))[11:16]) # Added dorm bc it seemed important
final_lm2_formula <- as.formula(
  paste("y ~", paste(final_lm2_predictors, collapse = " + "))
)
final_lm2_model = lm(final_lm2_formula, data_train)
save(final_lm2_model, file = "Modelos Nico/final_lm2_model.RData")
load("Modelos Nico/final_lm2_model.RData")

final_lm2_predictors <- labels(terms(final_lm2_model))

summary(final_lm2_model)
anova(final_lm2_model)
k_fold_cv_linear_model(final_lm2_model, data_train)
check_multicollinearity(final_lm2_model, data_train)
# Diagnostics
par(mfrow = c(2, 2))
plot(final_lm2_model)


########################### GAM Models #########################################
## Model 1 --- Manual selection after BIC + Tensorial geospace -----------------
final_gam1_predictors <- final_lm1_predictors
num_final_gam1_predictors <- final_gam1_predictors[1:6]
cat_final_gam1_predictors <- final_gam1_predictors[7:12]
inter_final_gam1_predictors <- final_gam1_predictors[13]
# We will omit latitud and radius and consider a bi-dimensional smooth term
# based in latitud and longitud.
#log.sup.util will be implemented through the interaction
num_final_gam1_predictors <- setdiff(num_final_gam1_predictors, c("latitud", "radius", "log.sup.util"))

final_gam1_model <- gam(y ~ te(latitud, longitud, k = c(20,20), bs = c('ps', 'ps')) +
                          s(ref.hip.zona, k = 15, bs = 'ps') +
                          s(Poca_limp, k = 15, bs = 'ps') +
                          s(Pocas_zonas, k = 15, bs = 'ps') +
                          s(log.sup.util, k = 15, bs = 'ps', by = comercial) +
                          dorm + 
                          banos +
                          ascensor +
                          estado +
                          comercial +
                          tipo.casa, method = "REML", data=data_train, select=FALSE)
summary(final_gam1_model)
save(final_gam1_model, file = "Modelos Nico/final_gam1_model.RData")
load("Modelos Nico/final_gam1_model.RData")

## Model 2 --- Manual selection after BIC --------------------------------------
final_gam2_model <- gam(y ~ latitud +
                          s(ref.hip.zona, k = 15, bs = 'ps') +
                          Poca_limp +
                          s(Pocas_zonas, k = 15, bs = 'ps') +
                          radius +
                          log.sup.util +
                          dorm + 
                          banos +
                          ascensor +
                          estado +
                          comercial +
                          tipo.casa +
                          log.sup.util:comercial, method = "REML", data=data_train, select=FALSE)
summary(final_gam2_model)
save(final_gam2_model, file = "Modelos Nico/final_gam2_model.RData")
load("Modelos Nico/final_gam2_model.RData")

## Model 3 ----------- Sparse AIC + Tensorial geospace -------------------------
final_lm2_predictors
# we disregard Pobl.0_14_div_Poblac.Total:comercial, tipo casa is more significant
# and log.sup.util:comercial, banos is more significant
final_gam3_model <- gam(y ~ te(latitud, longitud, k = c(20,20), bs = c('ps', 'ps')) +
                          s(ref.hip.zona, k = 15, bs = 'ps') +
                          s(antig, k = 15, bs = 'ps') +
                          s(Poca_limp, k = 15, bs = 'ps') +
                          s(Pobl.0_14_div_Poblac.Total, k = 15, bs = 'ps', by = tipo.casa) +
                          s(log.sup.util, k = 15, bs = 'ps', by = banos) +
                          dorm + 
                          banos +
                          ascensor +
                          estado +
                          comercial +
                          tipo.casa, method = "REML", data=data_train, select=FALSE)
summary(final_gam3_model)
save(final_gam3_model, file = "Modelos Nico/final_gam3_model.RData")
load("Modelos Nico/final_gam3_model.RData")


## Model 4 ----------- Sparse AIC ----------------------------------------------
final_lm2_predictors
# we disregard Pobl.0_14_div_Poblac.Total:comercial, tipo casa is more significant
# and log.sup.util:comercial, banos is more significant
final_gam4_model <- gam(y ~ s(latitud, k = 20, bs = 'ps') +
                          s(ref.hip.zona, k = 15, bs = 'ps') +
                          s(antig, k = 15, bs = 'ps') +
                          s(Poca_limp, k = 15, bs = 'ps') +
                          s(Pobl.0_14_div_Poblac.Total, k = 15, bs = 'ps', by = tipo.casa) +
                          s(radius, k = 20, bs = 'ps') +
                          s(log.sup.util, k = 15, bs = 'ps', by = banos) +
                          dorm + 
                          banos +
                          ascensor +
                          estado +
                          comercial +
                          tipo.casa, method = "REML", data=data_train, select=FALSE)
summary(final_gam4_model)
save(final_gam4_model, file = "Modelos Nico/final_gam4_model.RData")
load("Modelos Nico/final_gam4_model.RData")

## Best GAM ====================================================================
## The best (a priori) model is Model 4 ---> Let's linearize those terms which have
## edf < 2 : latitud, antig, poca_limp , radius and log.sup.util:banos
## The kept smooth terms are ref.hip.zonas and Pobl.0_14_div_Poblac.Total:tipo.casa
final_gam_model <- gam(y ~ latitud +
                         s(ref.hip.zona, k = 15, bs = 'ps') +
                         antig+
                         Poca_limp +
                         s(Pobl.0_14_div_Poblac.Total, k = 15, bs = 'ps', by = tipo.casa) +
                         radius+
                         log.sup.util +
                         dorm + 
                         banos +
                         ascensor +
                         estado +
                         comercial +
                         tipo.casa + 
                         log.sup.util:banos, method = "REML", data=data_train, select=FALSE)
summary(final_gam_model)
save(final_gam_model, file = "Modelos Nico/final_gam_model.RData")
load("Modelos Nico/final_gam_model.RData")

## Cross-Validation for GAMs
fit_gam_model = function(formula, data_train){
  formula <- as.formula(formula)
  model = gam(formula, method = "REML", data = data_train, select=TRUE)
  summary(model)
  return(model)
}



fit_gam_model(final_gam_model, data_train)

k_fold_cv_gam_model <- function(model_formula, 
                                data_train,
                                k=35){
  cat("=== Running k_fold Cross Validation --- GAM === \n")
  
  # Create the K-fold partition
  folded_data <- k_fold(data_train,k)$.folds
  
  # Initialize a vector to store each fold's rsme
  cv_rmse_y <- numeric(k)
  
  # Initialize a vector to store each fold's rsme
  cv_rmse_logy <- numeric(k)
  
  # Initialize a vector to store each fold's Rsq_adj
  cv_rsq_adj_logy <- numeric(k)
  
  for (i in 1:k){
    # Create the fold's test/train split
    temp_train <- data_train[which(folded_data!=i),]
    temp_test <- data_train[which(folded_data==i),]
    
    # Fit the model and make predictions
    temp_model <- fit_gam_model(model_formula, temp_train)
    temp_predictions <- predict(temp_model, newdata = temp_test)
    
    ## Calculate error metrics and store them
    
    # RsqAdj in log(y)
    n_test = nrow(temp_test)
    num_predictors = length(coefficients(temp_model)) 
    
    SSE = sum((temp_test$y - temp_predictions)^2)
    SST = sum((mean(temp_test$y) - temp_test$y)^2)
    
    cv_rsq_adj_logy[i] = 1 - (SSE/(n_test-num_predictors))/(SST/(n_test-1))
    
    # RMSE in log(y)
    cv_rmse_logy[i] <- sqrt(SSE/n_test)
    
    # RMSE in y
    SSE_exp = sum((exp(temp_test$y) - exp(temp_predictions))^2)
    cv_rmse_y[i] <- sqrt(SSE_exp/n_test)
  }
  
  # Return the vector with rmse for each k-fold
  return(list(cv_rmse_y=cv_rmse_y,
              mean_cv_rmse_y = mean(cv_rmse_y),
              cv_rmse_logy=cv_rmse_logy,
              mean_cv_rmse_logy = mean(cv_rmse_logy),
              cv_rsq_adj_logy=cv_rsq_adj_logy,
              mean_cv_rsq_adj_logy = mean(cv_rsq_adj_logy)
  ))
}

k_fold_cv_gam_model(final_gam_model, data_train)

plot(final_gam_model, se = 2, shade =TRUE, resid = TRUE, pages = 1)

########################### RIDGE Models #######################################
### Ridge Regression Example with Prediction
library(glmnet)
library(readxl)
library(dplyr)

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

# Read and preprocess the training and test datasets
data_train <- read_excel("Data/data_train.xlsx") %>% preprocess()
data_test <- read_excel("Data/data_test.xlsx") %>% preprocess()
data_test$test_indices = NULL
set.seed(248)
# Generate regression matrix for training
X_train <- model.matrix(y ~ . - 1, data = data_train)

# Train Ridge Regression model (alpha = 0)
fit.ridge <- glmnet(X_train, data_train$y, alpha = 0)
par(mfrow=c(1,2))
plot(fit.ridge,xvar="lambda")

# Cross-validation to determine optimal lambda
cv.out <- cv.glmnet(X_train, data_train$y, alpha = 0)  # alpha=0 for Ridge
plot(cv.out)
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
    
    # Find optimal lambda using cross-validation
    cv_ridge <- cv.glmnet(X_train, y_train, alpha = alpha)
    opt_lambda <- cv_ridge$lambda.min
    
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


library(readxl)
data_test <- read_excel("Data/data_test.xlsx")
preprocess_test = function(data,
                           predictors){
  
  # Eliminate
  data$test_indices <- NULL
  
  # Eliminate 
  data$barrio <- NULL
  
  # Eliminate 
  data$cod_barrio <- NULL
  
  # Eliminate 
  data$cod_distrito <- NULL
  
  # Logarithmic objective variable
  # data$y <- log(data$precio.house.m2) 
  # data$precio.house.m2 <- NULL # Eliminate the old variable
  
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
  
  data <- subset(data, select=predictors)
  
  return(data)
}

data_test <- preprocess_test(data_test)
str(data_test)
## Predictions
#### LM1
LM1_prediction <- predict(final_lm1_model, data_test)
LM1_prediction


#### LM2
LM2_prediction <- predict(final_lm2_model, data_test)
LM2_prediction

# write predictions to excel
## Best Model
wb <- loadWorkbook("Data/predicted_prices.xlsx")
writeData(wb, sheet = "Hoja1", x = exp(LM2_prediction), startCol = 2, startRow = 2)
saveWorkbook(wb,"Data/predicted_prices.xlsx",overwrite=TRUE)


#### Ridge
Ridge_prediction <- predict(final_Ridge_model, data_test)
Ridge_prediction

#### GAM
GAM_prediction <- predict(final_gam_model, data_test)
GAM_prediction




########################### Pipeline ###########################################
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