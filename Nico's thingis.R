library(readxl)
library(leaps)
library(caret)
library(groupdata2)
library(car)
library(dplyr)
library(mgcv)
library(MASS)
library(geosphere)

## Check Multicoliniearity -----------------------------------------------------
check_multicollinearity <- function(model, data) {
  
  # Identify numerical and categorical variables
  predictors <- labels(terms(model)) # Variables used in the model
  data <- data[predictors]
  num_id <- sapply(data, is.numeric)
  num_vars <- names(data)[num_id]
  cat_vars <- names(data)[!num_id]
  
  # Model Matrix
  X <- data[,num_vars]
  R <- cor(X)
  
  # Calculate condition number using kappa
  condition_number <- kappa(R, exact = TRUE)
  
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

data <- read_excel("Data/data_train.xlsx")

# radius
# Central point (Puerta del Sol)
center <- c(-3.7038, 40.4168)

# Calculate distances and add a new column
data$radius <- distHaversine(
  matrix(c(data$longitud, data$latitud), ncol = 2),
  matrix(rep(center, nrow(data)), ncol = 2, byrow = TRUE)
) / 1000  # Convert meters to kilometers

# distrito
data$distrito[data$distrito %in% c("carabanchel", "puente_vallecas", "usera","vallecas","villaverde")] = "south"
data$distrito[data$distrito %in% c("arganzuela", "centro", "chamberi","retiro","salamanca")] = "centro"
data$distrito[data$distrito %in% c("barajas", "chamartin", "fuencarral", "hortaleza", "tetuan")] = "north"
data$distrito[data$distrito %in% c("moncloa","latina")] = "west"
data$distrito[data$distrito %in% c("vallecas","moratalaz","vicalvaro","san_blas","ciudad_lineal")] = "east"

# dorm
data$dorm[data$dorm %in% c("0","1")] = "0&1"
data$dorm[data$dorm %in% c("3","4")] = "3&4"
data$dorm[data$dorm %in% c("5","6","7","8","9","10")] = "5+"

#banos
data$banos[data$banos %in% c("3","4","5","6","7","8")] = "3+"

# type
data$tipo.casa[data$tipo.casa %in% c("Otros","piso")] = "piso"
data$tipo.casa[data$tipo.casa %in% c("chalet","duplex")] = "chalet+duplex"
data$tipo.casa[data$tipo.casa %in% c("atico","estudio")] = "atico+estudio"

# state
data$estado[data$estado %in% c("excelente","nuevo-semin,","reformado")] = "excelente+nuevo-semin+reformado"
data$estado[data$estado %in% c("buen_estado","segunda_mano")] = "buen_estado+segunda_mano"
data$estado[data$estado %in% c("a_reformar","reg,-mal")] = "a_reformar+reg-mal"

# normalize latitude and longitude
data$longitud <- (data$longitud - mean(data$longitud))/sd(data$longitud)
data$latitud <- (data$latitud - mean(data$latitud))/sd(data$latitud)

data$train_indices <- NULL
data$log.precio.house.m2 <- log(data$precio.house.m2) 
data$precio.house.m2 <- NULL
data$barrio <- NULL
data$cod_barrio <- NULL
data$cod_distrito <- NULL
data$sup.const <- NULL
factor_columns <- c("distrito", "dorm", "banos", "tipo.casa", "inter.exter", 
                    "ascensor", "estado", "comercial", "casco.historico", "M.30")
data[factor_columns] <- lapply(data[factor_columns], as.factor)


num_id <- sapply(data, is.numeric)
num_vars <- names(data)[num_id]
num_vars
cat_vars <- names(data)[!num_id]
cat_vars

data_train <- data
data_train$y <- data_train$log.precio.house.m2
data_train$log.precio.house.m2 <- NULL
num_id <- sapply(data_train, is.numeric)
num_vars <- setdiff(names(data_train)[num_id],"y")
predictors <- c("ref.hip.zona", "antig", "Poca_limp", "PM10", "Pobl.0_14_div_Poblac.Total" ,   "PoblJubilada_div_Poblac.Total", "Inmigrantes.porc", "Pocas_zonas")
cat_vars = factor_columns


#### No interactions
lm_formula <- as.formula(
  paste("y ~", paste(c(num_vars, cat_vars), collapse = " + "))
)
lm_model = lm(lm_formula,data = data_train)
summary(lm_model)
check_multicollinearity(lm_model, data = data_train)

### Step BIC
n <- 736
lm_BIC <- stepAIC(lm_model, direction = 'both', k = log(n))
summary(lm_BIC)
check_multicollinearity(lm_BIC, data = data_train)

### Step AIC
lm_AIC <- stepAIC(lm_model, direction = 'both', k = 2)
summary(lm_AIC)
check_multicollinearity(lm_AIC, data = data_train)

# Create the formula with p-splines for numerical vars. and straight categorical vars.
predictors <- labels(terms(lm_BIC))
num_id <- sapply(data_train[predictors], is.numeric)
num_vars <- names(data_train[predictors])[num_id]
cat_vars <- names(data_train[predictors])[!num_id]

# Create a fucntion to automatically normalize the numerical vars
normalize = function(row){
  row = (row - mean(row))/sd(row)
  return(row)
}

data_train[num_vars] = apply(data_train[num_vars], 2, normalize)

gam_formula <- as.formula(
  paste("y ~", paste(c(paste0("s(", num_vars, ", bs='ps', m = 3)"),cat_vars), collapse = " + "))
)
# Fit the GAM 
gam_model <- gam(gam_formula, data = data_train)
summary(gam_model)


### ALL INTERACTIONS

num_id <- sapply(data_train, is.numeric)
num_vars <- setdiff(names(data_train)[num_id], "y")
num_vars
cat_vars <- names(data_train)[!num_id]
cat_vars

interact_lm_formula <- as.formula(
  paste("y ~", "(", paste(num_vars, collapse = " + "), ")", ":", "(", paste(cat_vars, collapse = " + "), ")" )
)
interact_lm_model = lm(interact_lm_formula,data = data_train)
summary(interact_lm_model)

interact_lm_BIC <- stepAIC(interact_lm_model, direction = 'both', k = log(n))
summary(interact_lm_BIC)

# Save the model to a file and load it
save(interact_lm_BIC, file = "interact_lm_BIC.RData")
load("interact_lm_BIC.RData")
interact_predictors <- labels(terms(interact_lm_BIC))

### Full lm model BIC vars
predictors <- labels(terms(lm_BIC))
num_id <- sapply(data_train[predictors], is.numeric)
num_vars <- names(data_train[predictors])[num_id]
cat_vars <- names(data_train[predictors])[!num_id]
interact_predictors <- labels(terms(interact_lm_BIC))

full_lm_formula <- as.formula(
  paste("y ~", paste(c(num_vars, cat_vars, interact_predictors), collapse = " + "))
)
full_lm_model = lm(full_lm_formula,data = data_train)
summary(full_lm_model)
# BIC
full_lm_BIC <- stepAIC(full_lm_model, direction = 'both', k = log(n))
summary(full_lm_BIC)
# AIC
full_lm_AIC <- stepAIC(full_lm_model, direction = 'both', k = 2)
summary(full_lm_AIC)


full_gam_formula <- as.formula(
  paste("y ~", paste(c(paste0("s(", num_vars, ", bs='ps', m = 3)"),cat_vars, interact_predictors), collapse = " + "))
)
# Fit the GAM 
full_gam_model <- gam(full_gam_formula, data = data_train)
summary(full_gam_model)

# if (!requireNamespace("gam")) install.packages("gam")
# library(gam)
# aux_gam_formula <- as.formula(
#   paste("~", paste(c(paste0("s(", num_vars, ", bs='ps', m = 3)"),cat_vars, interact_predictors), collapse = " + "))
# )
# full_gam_model_AIC <- step.gam(full_gam_model, scope = list(
#   lower = ~ 1,        # Minimum model
#   upper = aux_gam_formula  # Full model
# ), direction = "both")
# 
# full_gam_model_AIC <- stepAIC(full_gam_model, direction = 'both', k = 2)
# summary(full_gam_model_AIC)

