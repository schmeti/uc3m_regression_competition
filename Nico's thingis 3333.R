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

#### WARNING --- IN PROGRESS
### Loop to find a comprehensively balanced seed for the k-fold
seed <- 1 
mix <- 1000000
k = 4
Tot_table <- list()
n <- 736

num_id <- sapply(data_train, is.numeric)
num_vars <- setdiff(names(data_train)[num_id], "y")
cat_vars <- names(data_train)[!num_id]

for (var in cat_vars){
  Tot_table[[as.name(var)]] = table(data_train[[as.name(var)]])/n
}
for (i in 1:2000){
  set.seed(i)
  folded_data <- fold(data_train, 
                      k = k, 
                      cat_col = "tipo.casa",
                      num_col = "y")
  mix_aux <- 0
  for (j in 1:k){
    temp_indexes <- which(folded_data$.folds == j)
    ll <- length(temp_indexes)
    for(var in cat_vars){
      mix_aux= mix_aux + sum(abs(table(data_train[[as.name(var)]][which(folded_data$.folds == j)])/ll - Tot_table[[as.name(var)]]))
    }
  }
  print(i)
  print(mix_aux)
  if (mix_aux < mix){
    seed <- i
    mix <- mix_aux
  }
}
print(seed) # Up until 2000 ---> 248 is the best (k=4)
for (j in 1:k){
  print(paste(j, "- Fold   ==================================================="))
  for(var in cat_vars){
    print(paste(var,"----------------------------------------------------------"))
    print(table(data_train[[as.name(var)]][which(folded_data$.folds == j)])/ll)  
    print(Tot_table[[as.name(var)]])
  }
}

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
  cv_rmse <- numeric(k)
  
  # Initialize a vector to store each fold's Rsq_adj
  cv_rsq_adj <- numeric(k)
  
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
    
    cv_rsq_adj[i] = 1 - (SSE/(n_test-num_predictors))/(SST/(n_test-1))
    
    # RMSE in the original variable
    SSE_exp = sum((exp(temp_test$y) - exp(temp_predictions))^2)
    cv_rmse[i] <- sqrt(SSE_exp/n_test)
  }
  
  # Return the vector with rmse for each k-fold
  return(list(cv_rmse=cv_rmse,
              mean_cv_rmse = mean(cv_rmse),
              cv_rsq_adj=cv_rsq_adj,
              mean_cv_rsq_adj = mean(cv_rsq_adj)
  ))
}




#### Modelling: No interactions ------------------------------------------------
## Base
num_id <- sapply(data_train, is.numeric)
num_vars <- setdiff(names(data_train)[num_id], "y")
num_vars
cat_vars <- names(data_train)[!num_id]
cat_vars

lm_formula <- as.formula(
  paste("y ~", paste(c(num_vars, cat_vars), collapse = " + "))
)
lm_model = lm(lm_formula,data = data_train)
summary(lm_model)

## Step BIC
n <- 736
lm_BIC <- stepAIC(lm_model, direction = 'both', k = log(n))
summary(lm_BIC)
BIC_predictors <- labels(terms(lm_BIC))
total_lm_formula <- as.formula(
  paste("y ~", "(", paste(num_vars, collapse = " + "), ")", "*", "(", paste(cat_vars, collapse = " + "), ")" )
)


### Modeling: Interactions -----------------------------------------------------
## Base
total_lm_formula <- as.formula(
  paste("y ~", "(", paste(num_vars, collapse = " + "), ")", "*", "(", paste(cat_vars, collapse = " + "), ")" )
)
total_lm_model = lm(total_lm_formula,data = data_train)
summary(total_lm_model)
k_fold_cv_linear_model(total_lm_formula, data_train) # Errores de multicolinearidad

## Step BIC
n <- 736
#total_lm_BIC <- stepAIC(total_lm_model, direction = 'both', k = log(n))
load("Modelos Nico 3/total_lm_BIC.RData")
summary(total_lm_BIC)
#save(total_lm_BIC, file = "Modelos Nico 3/total_lm_BIC.RData")
total_BIC_predictors <- labels(terms(total_lm_BIC))
total_BIC_formula <- as.formula(
  paste("y ~", paste(total_BIC_predictors, collapse = " + "))
)
k_fold_cv_linear_model(total_BIC_formula, data_train)


total_BIC_interactions <- total_BIC_predictors[16:length(total_BIC_predictors)]


# Create plots for each interaction
plots <- lapply(total_BIC_interactions, function(interaction) {
  # Split the interaction into individual variables
  vars <- unlist(strsplit(interaction, ":"))
  
  ggplot(data_train, aes(x = !!as.name(vars[1]), y = y, color = !!as.name(vars[2]))) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = paste("Interaction:", interaction),
         x = vars[1], y = "y") +
    theme_minimal()
})
# We disregard PM10:comercial, pob.14:comercial
# because their interactions seem unimportant ---> Fit the model without them
# and check if PM10 and pob.14 are significative
manual_BIC_predictors <- setdiff(labels(terms(total_lm_BIC)), c("PM10:comercial", "Pobl.0_14_div_Poblac.Total:comercial"))
manual_BIC_formula <- as.formula(
  paste("y ~", paste(manual_BIC_predictors, collapse = " + "))
)
manual_BIC_model <- lm(manual_BIC_formula, data = data_train)
summary(manual_BIC_model)
# We disregard PoblJubilada_div_Poblac.Total, PM10
manual_BIC_predictors <- setdiff(labels(terms(manual_BIC_model)), c("PoblJubilada_div_Poblac.Total", "PM10"))
manual_BIC_formula <- as.formula(
  paste("y ~", paste(manual_BIC_predictors, collapse = " + "))
)
manual_BIC_model <- lm(manual_BIC_formula, data = data_train)
summary(manual_BIC_model)
k_fold_cv_linear_model(manual_BIC_formula, data_train)
### This is the sparser model yet


# Identify the variables included in teh BIC model without interactions and the manual one
not_included <- setdiff(BIC_predictors, manual_BIC_predictors)
not_included

# Add them (and interactions) to the manual model and perform BIC again
manual_BIC_predictors
num_BIC_predictors <- manual_BIC_predictors[1:8]
cat_BIC_predictors <- manual_BIC_predictors[9:13]
inter_BIC_predictors <- manual_BIC_predictors[14:16]

# Get all the interactions to be added
not_included_num <- not_included[c(1,2)]
not_included_cat <- not_included[3]

combinations1 <- expand.grid(not_included_num, cat_BIC_predictors)
inter_new1 <- paste(combinations1$Var1, combinations1$Var2, sep = ":")
combinations2 <- expand.grid(num_BIC_predictors, not_included_cat)
inter_new2 <- paste(combinations2$Var1, combinations2$Var2, sep = ":")

added_terms <- c(not_included, inter_new1, inter_new2)

added_formula <- as.formula(
  paste("y ~", paste(c(manual_BIC_predictors, added_terms), collapse = " + "))
)
added_model <- lm (added_formula, data_train)
summary(added_model)

# Perform BIC again
added_BIC_model <- stepAIC(added_model, direction = 'both', k = log(n))
summary(added_BIC_model)
k_fold_cv_linear_model(added_formula, data_train) # Worse

### New vars after BIC
new_vars <- setdiff(labels(terms(added_BIC_model)), labels(terms(manual_BIC_model)))
new_vars

new_manual_BIC_formula <- as.formula(
  paste("y ~", paste(c(manual_BIC_predictors, new_vars), collapse = " + "))
)
new_manual_BIC_model <- lm(new_manual_BIC_formula, data = data_train)
summary(new_manual_BIC_model)
k_fold_cv_linear_model(new_manual_BIC_formula, data_train)
