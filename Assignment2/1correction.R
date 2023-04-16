
# library(tidyverse)
library(forcats)
library(caret)
library(mlr)
library(dplyr)
library(forcats)
library(glmnet)
library(randomForest)
library(pROC)

# Load the data
MLData2023 <- read.csv("MLData2023.csv")

# Clean the data
MLData2023_cleaned <- MLData2023 %>%
  # Remove ID column
  select(-ID) %>%
  # Remove highly correlated columns
  select(-c(Total.Bytes.Sent, Total.Bytes.Received, Total.Bytes)) %>%
  # Remove highly skewed columns
  select(-c(Bytes.Sent.Std, Bytes.Received.Std)) %>%
  # Remove columns with too many missing values
  select(-c(Bytes.Sent.Median, Bytes.Received.Median)) %>%
  # Remove rows with missing values
  na.omit() %>%
  # Filter for Class = 0 or 1
  filter(Class == 0 | Class == 1) %>%
  # Merge Windows categories
  mutate(Operating.System = fct_collapse(Operating.System, Windows_All = c("Windows 7", "Windows 10+", "Windows (Unknown)"))) %>%
  # Merge other categories
  mutate(Operating.System = fct_collapse(Operating.System, Others = c("-","Android","iOS", "Linux (unknown)")),
         Connection.State = fct_collapse(Connection.State, Others = c("INVALID", "NEW", "RELATED"))) %>%

  # Convert Class to factor
  mutate(Class = factor(Class, labels = c("NonMal", "Mal")))

# Generate training and testing datasets
set.seed(10550239))

# Separate samples of non-malicious and malicious events 
dat.class0 <- MLData2023_cleaned %>% filter(Class == "NonMal") # non-malicious 
dat.class1 <- MLData2023_cleaned %>% filter(Class == "Mal") # malicious 

# Randomly select 19800 non-malicious and 200 malicious samples, then combine them to form the training samples 
rows.train0 <- sample(1:nrow(dat.class0), size = 19800, replace = FALSE) 
rows.train1 <- sample(1:nrow(dat.class1), size = 200, replace = FALSE) 

# Your 20000 unbalanced training samples 
train.class0 <- dat.class0[rows.train0,] # Non-malicious samples 
train.class1 <- dat.class1[rows.train1,] # Malicious samples 
mydata.ub.train <- rbind(train.class0, train.class1) 
mydata.ub.train <- mydata.ub.train %>% 
                                    mutate(Class = factor(Class, labels = c("NonMal","Mal")))

# Your 39600 balanced training samples, i.e. 19800 non-malicious and malicious samples each.
train.class1_2 <- train.class1[sample(1:nrow(train.class1), size = 19800, replace = TRUE),] 
mydata.b.train <- rbind(train.class0, train.class1_2) 
mydata.b.train <- mydata.b.train %>%         
                                    mutate(Class = factor(Class, labels = c("NonMal","Mal")))

# Your testing samples 
test.class0 <- dat.class0[-rows.train0,] 
test.class1 <- dat.class1[-rows.train1,] 
mydata.test <- rbind(test.class0, test.class1) 
mydata.test <- mydata.test %>% 
                            mutate(Class = factor(Class, labels = c("NonMal","Mal")))

# Set seed
set.seed(10550239)

# Define the two models
models.list1 <- c("Logistic Ridge Regression", "Logistic LASSO Regression", "Logistic Elastic-Net Regression")
models.list2 <- c("Classification Tree", "Bagging Tree", "Random Forest")
myModels <- c(sample(models.list1, size = 1), sample(models.list2, size = 1))
myModels

# Train the models on the unbalanced dataset
# First, prepare the data for modeling
mydata.ub.train$Class <- as.factor(mydata.ub.train$Class)
mydata.test$Class <- as.factor(mydata.test$Class)

# Define the trainControl object for tuning
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

# Model 1: Logistic Ridge Regression
if (myModels[1] == "Logistic Ridge Regression") {
    # Define the hyperparameter grid
    hyperparams <- expand.grid(alpha = 0:10/10, lambda = c(0, 0.01, 0.1, 1, 10))
    
    #  Train the model with cross-validation and hyperparameter tuning
    model <- train(Class ~ ., data = mydata.ub.train, method = "glmnet", trControl = ctrl, tuneGrid = hyperparams)
    
    # Make predictions on the testing set
    preds <- predict(model, newdata = mydata.test)
    
    # Evaluate the model performance
    confusionMatrix(preds, mydata.test$Class)
}

# Model 2: Random Forest
if (myModels[2] == "Random Forest") {
    # Define the hyperparameter grid
    hyperparams <- list(num.trees = c(50, 100, 150), mtry = c(3, 5, 7))
    
    # Train the model with cross-validation and hyperparameter tuning
    model <- train(Class ~ ., data = mydata.ub.train, method = "rf", trControl = ctrl, tuneGrid = hyperparams)
    
    # Make predictions on the testing set
    preds <- predict(model, newdata = mydata.test)
    
    # Evaluate the model performance
    confusionMatrix(preds, mydata.test$Class)
}

# Train the models on the balanced dataset
# First, prepare the data for modeling
mydata.b.train$Class <- as.factor(mydata.b.train$Class)

# Define the trainControl object for tuning
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

# Model 1: Logistic Ridge Regression
if (myModels[1] == "Logistic Ridge Regression") {
    # Define the hyperparameter grid
    hyperparams <- expand.grid(alpha = 0:10/10, lambda = c(0, 0.01, 0.1, 1, 10))
    
    # Train the model with cross-validation and hyperparameter tuning
    model <- train(Class ~ ., data = mydata.b.train, method = "glmnet", trControl = ctrl, tuneGrid = hyperparams)
    
    # Make predictions on the testing set
    preds <- predict(model, newdata = mydata.test)
    
    # Evaluate the model performance
    confusionMatrix(preds, mydata.test$Class)
}

# Model 2: Random Forest
if (myModels[2] == "Random Forest") {

    # Define the hyperparameter grid
    hyperparams <- list(num.trees = c(50, 100, 150), mtry = c(3, 5, 7))

    # Train the model with cross-validation and hyperparameter tuning
    model <- train(Class ~ ., data = mydata.b.train, method = "rf", trControl = ctrl, tuneGrid = hyperparams)

    # Make predictions on the testing set
    preds <- predict(model, newdata = mydata.test)

    # Evaluate the model performance
    confusionMatrix(preds, mydata.test$Class)
}

# Train the logistic regression model
logistic_model <- glm(Class ~ ., data = mydata.b.train, family = binomial())

# Predict the class probabilities on the test set
logistic_probabilities <- predict(logistic_model, newdata = mydata.test, type = "response")

# Compute the AUC of the logistic regression model
logistic_auc <- roc(mydata.test$Class, logistic_probabilities)$auc

# Train the random forest model
rf_model <- randomForest(Class ~ ., data = mydata.b.train, ntree = 500, importance = TRUE)

# Predict the class probabilities on the test set
rf_probabilities <- predict(rf_model, newdata = mydata.test, type = "prob")[,2]

# Compute the AUC of the random forest model
rf_auc <- roc(mydata.test$Class, rf_probabilities)$auc

# Print the AUC values
print(paste0("AUC of logistic regression: ", round(logistic_auc, 4)))
print(paste0("AUC of random forest: ", round(rf_auc, 4)))
