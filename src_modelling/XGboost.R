# Load necessary library
library(xgboost)

data_train <- read_excel("Data/data_train.xlsx")
#data_train$y = data_train$precio.house.m2   
data_train = preprocess(data_train)


data_train$precio.house.m2   = NULL
# Load necessary libraries
library(Matrix)
library(dplyr)


X_train <- data_train[, !names(data_train) %in% "y"] # Features
y_train <- data_train$y # Target variable


# Perform one-hot encoding for categorical variables
X_train_encoded <- model.matrix(~ . - 1, data = X_train) # One-hot encoding (drop intercept)

# Convert to DMatrix
dtrain <- xgb.DMatrix(data = X_train_encoded, label = y_train)

# Define model parameters
params <- list(
  objective = "reg:squarederror",  # Use "binary:logistic" for classification
  eta = 0.1,                      # Learning rate
  max_depth = 6,                  # Depth of trees
  subsample = 0.8,                # Fraction of rows to sample
  colsample_bytree = 0.8          # Fraction of features to sample
)

# Train the XGBoost model
set.seed(123) # For reproducibility
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,                  # Number of boosting iterations
  watchlist = list(train = dtrain), # Monitoring training error
  verbose = 1
)

# Make predictions
predictions <- predict(xgb_model, newdata = X_train_encoded)

# Print feature importance
importance_matrix <- xgb.importance(model = xgb_model)
options(max.print = 10000000)  # Set a high limit for printing

print(importance_matrix)
