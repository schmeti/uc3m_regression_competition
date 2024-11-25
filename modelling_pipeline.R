library(readxl)
library(leaps)
library(caret)
library(lubridate)


# Test/Train Split
train_test_split <- function(data) {
  train_index <- createDataPartition(data$precio.house.m2, p = 0.7, list = FALSE)
  data_train <- data[train_index, ]
  data_test <- data[-train_index, ]
  return(list(data_train = data_train, data_test = data_test))
}

# K-Fold -----------------------------------------------------------------------
if(!require(groupdata2)) install.packages("groupdata2")
library(groupdata2)

k_fold <- function(data, k = 10) {
  # Identify numerical and categorical variables
  num_id <- sapply(data, is.numeric)
  num_vars <- names(data)[num_id]
  cat_vars <- names(data)[!num_id]
  
  # Create a k-fold partition with balanced cat_vars and which
  # tries to minimize similar values in "precio.house.m2"
  folded_data <- fold(data, k = k, cat_col = cat_vars, num_col = "precio.house.m2")
  # It adds a new variable, .folds, which assigns a value 1 to k to each
  # instance, dividing them by folds
  
  # Return the new dataset
  return(folded_data)
}


# K-fold cross-validation ------------------------------------------------------
k_fold_cv <- function(model, data_train, k =10){
    
  # Create the K-fold partition
  folded_data <- k_fold(data_train, k = k)$.fold
  
  # Initialize a vector to store each fold's rsme
  cv_rmse <- numeric(k)
    
  for (i in 1:k){
    train_id <- (1:k)[-i] # Train folds
    test_id <- i          # Test folds
    
    # Create the fold's test/train split
    temp_train <- data_train[which(folded_data!=i),]
    temp_test <- data_train[which(folded_data==i),]
    
    # Fit the model and make predictions
    temp_model <- fit_model(temp_train)
    temp_predictions <- predict(model, newdata = temp_test)
    
    # Calculate error metrics and store them
    cv_rmse[i] <- sqrt(mean((predictions - temp_test$precio.house.m2)^2))
  }
  
  # Return the vector with rmse for each k-fold
  return(cv_rmse)
}
# ------------------------------------------------------------------------------




# Preprocess
preprocess = function(data){
  data_processesd = data
  return(data_processesd)
}

# Fit Model
fit_model = function(data_train){
  model = lm(precio.house.m2~.,data = data_train)
  return(model)
}

# Predict and score
predict_and_score = function(model, data_test, print = FALSE) {
  # predict
  predictions = predict(model, newdata = data_test)
  results = data_test %>% mutate(Predicted = predictions)

  #score
  rmse = sqrt(mean((predictions - data_test$precio.house.m2)^2))
  
  score = list(rmse = rmse)
  
  return(score)
}

### Run Pipeline
run_pipeline = function(data, 
                        store_model = FALSE,
                        store_model_name = "linear_model",
                        load_model_path = ""){
  
  ### pipeline
  
  data_train = data
  data_test = data
  
  # preprocess
  data_processesd = preprocess(data = data_train)
  
  # fit/load model
  load_model_path <- trimws(load_model_path)
  if (!is.null(load_model_path) && load_model_path != "") {
    model = readRDS(load_model_path)
    print("Model loaded successfully")
  } else {
    print("Fitting a new model.")
    model = fit_model(data_train = data_processesd)
  }
  

  
  # predict and score
  score = predict_and_score(model = model,data_test = data_test)
  
  ### further features
  
  # save model
  if(store_model){
    # Save model with a properly constructed path using file.path()
    #model_file_path <- file.path("models", paste0(store_model_name, "_", gsub("[:/ ]", "_", Sys.time()), ".rds"))
    current_time <- Sys.time()
    model_file_path <- file.path("models", paste0(store_model_name, "_",gsub(" ","_",as.POSIXct(format(current_time, "%Y-%m-%d %H-%M"))),".rds"))
    saveRDS(model, file = model_file_path)
    print(paste("Model saved to", model_file_path))
  }

  
  print(score)
}

# execute
data <- read_excel("Data/data_train.xlsx")
run_pipeline(data,
             store_model = F,
             load_model_path = "models/linear_model_2024-11-25.rds"
             )

