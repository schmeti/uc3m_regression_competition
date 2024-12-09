### Ridge Example
library(glmnet)
data_train <- read.csv("C:/MARCOS/MASTER IN STATISTICS FOR DATA SCIENCE/GITHUB/uc3m_regression_competition/data_transformed.csv", header=TRUE, sep = ",")

# Calculate the regression matrix without the intercept
X = model.matrix(precio.house.m2 ~.-1, data = data_train)

# The function `glmnet` will chose a range of values of the ridge parameter
# We need to specify alpha=0
fit.ridge = glmnet(X, data_train$precio.house.m2, alpha = 0)

# Plot estimated coefficients for different values of the ridge parameter
plot(fit.ridge,xvar="lambda",label=TRUE)

# Calculate optimal ridge parameter via cross-validation
# Plot mean squared error versus ridge parameter (in the log scale)
cv.out = cv.glmnet(X, data_train$precio.house.m2, alpha = 0)  #alpha=0 means Ridge Regression
plot(cv.out)

# Getting optimal value for lambda
opt_lambda <- cv.out$lambda.min
opt_lambda

# Predicted values in data_train
predicted_values <- predict(fit.ridge, newx = X ,type = "response", s = opt_lambda)

## R-squared
# Variable respuesta real
actual_values <- data_train$precio.house.m2

# SSE and SST
SSE <- sum((actual_values - predicted_values)^2)
SST <- sum((actual_values - mean(actual_values))^2)

# R
R2 <- 1 - (SSE / SST)
R2

# R2 adjusted
n <- nrow(X)
p <- ncol(X)
R2_adj <- 1 - (1 - R2) * (n - 1) / (n - p - 1)
R2_adj

# Coefficients
coef_ridge <- predict(fit.ridge, type = "coefficients", s = opt_lambda)
print(coef_ridge)

