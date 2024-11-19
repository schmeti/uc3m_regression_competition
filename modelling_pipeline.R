library(readxl)
library(leaps)
library(caret)

# Test/Train Split
train_test_split <- function(data) {
  train_index <- createDataPartition(data$precio.house.m2, p = 0.7, list = FALSE)
  data_train <- data[train_index, ]
  data_test <- data[-train_index, ]
  return(list(data_train = data_train, data_test = data_test))
}

# K-Fold 
k_fold <- function(data, k = 10) {
  train_index <- createDataPartition(data$precio.house.m2, p = 0.7, list = FALSE)
  data_train <- data[train_index, ]
  data_test <- data[-train_index, ]
  return(list(data_train = data_train, data_test = data_test))
}

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
run_pipeline = function(data){
  
  # TODO: Fix train,test split
  #data_split = train_test_split(data)
  #data_train = data_split$data_train
  #data_test = data_split$data_test
  
  data_train = data
  data_test = data
  
  
  # preprocess
  data_processesd = preprocess(data = data_train)
  
  # fit model
  model = fit_model(data_train = data_processesd)
  
  # predict and score
  score = predict_and_score(model = model,data_test = data_test)
  
  print(score)
}

# execute
data <- read_excel("Data/data_train.xlsx")
run_pipeline(data)

