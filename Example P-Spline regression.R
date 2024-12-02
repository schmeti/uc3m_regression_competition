install.packages("mgcv")  # Install if not already done
library(mgcv)

# Example dataset
set.seed(123)
data <- data.frame(
  y = rnorm(1000),
  x1 = rnorm(1000),       # Continuous predictor
  x2 = runif(1000),       # Another continuous predictor
  cat_var = as.factor(sample(letters[1:3], 1000, replace = TRUE))  # Categorical predictor
)

# P-spline regression model NOT SPECIFYING knots or degree (lambda optimized through cv)
model <- gam(y ~ s(x1, bs = "ps") + s(x2, bs = "ps") + cat_var, data = data)

# Summary of the model (NOTE that in this exampel y is indip of x1, x2, thats teh reason for the bad results)
summary(model)

plot(model)      # Diagnostic plots for smooth terms
gam.check(model) # Further checks for GAM

# Predictions
new_data <- data.frame(x1 = rnorm(10), x2 = runif(10), cat_var = factor("a", levels = c("a", "b", "c")))
predict(model, newdata = new_data)

### 
# P-spline regression model SPECIFYING knots or degree (lambda optimized through cv)
model <- gam(y ~ s(x1, bs = "ps", k = 40, m = 3) + s(x2, bs = "ps", k = 40, m = 3) + cat_var, data = data)
summary(model)
plot(model)
###

# Interesting example
data <- data.frame(
  x1 = runif(1000),        # Continuous predictor
  x2 = runif(1000),        # Another continuous predictor
  cat_var = as.factor(sample(letters[1:3], 1000, replace = TRUE))  # Categorical predictor
)

# Define the coefficients for the categorical variable levels
coef_cat <- c(a = 1, b = 0.5, c = 0)

# Compute the response variable y
data$y <- sin(data$x1) + data$x2^2 + coef_cat[data$cat_var] + rnorm(1000, sd = 0.1)  # Gaussian error

# P-spline regression model SPECIFYING knots or degree (lambda optimized through cv)
model <- gam(y ~ s(x1, bs = "ps", k = 40, m = 3) + s(x2, bs = "ps", k = 40, m = 3) + cat_var, data = data)
summary(model)
plot(model)




