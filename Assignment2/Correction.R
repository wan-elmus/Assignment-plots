
# Load required packages
library(dplyr)
library(forcats)
library(glmnet)
library(randomForest)
library(pROC)
library(caret)

# import the dataset 
MLData2023=read.csv("MLData2023.csv", stringsAsFactors = TRUE)
# MLData2023 <- read.csv("MLData2023.csv", header = TRUE)

# Remove any duplicates
MLData2023 <- distinct(MLData2023)

# Filter the data to only include cases labeled with Class = 0 or 1
MLData2023_filtered <- MLData2023 %>% filter(Class == 0 | Class == 1)

MLData2023_filtered$Operating.System <- as.factor(MLData2023_filtered$Operating.System)

# # Check current levels of the Operating.System factor variable
# levels(MLData2023_filtered$Operating.System)

# Merge the three Windows categories together to form a new category named Windows_All and to merge iOS, Linux (Unknown), and Other to form the new category named Others
MLData2023_filtered$Operating.System <- fct_collapse(MLData2023_filtered$Operating.System,
                                                    Windows_All = c("Windows 7", "Windows 10+", "Windows (Unknown)"),
                                                    Others = c("-","Android","iOS", "Linux (unknown)"))


# Merge INVALID, NEW, and RELATED to form the new category named Others for the feature Connection.State
MLData2023_filtered$Connection.State <- fct_collapse(MLData2023_filtered$Connection.State,
                                                    Others = c("INVALID", "NEW", "RELATED"))

# # Check which variables have missing values
# missing_values <- colSums(is.na(MLData2023_filtered))
# print(missing_values)

# Impute missing values using makeX() function with median imputation
MLData2023_filtered <- makeX(MLData2023_filtered, method = "median")

# Select only the complete cases and name the cleaned dataset as MLData2023_cleaned
MLData2023_cleaned <- na.omit(MLData2023_filtered)

# Convert MLData2023_cleaned to a dataframe
MLData2023_cleaned <- as.data.frame(MLData2023_cleaned)

# Separate samples of non-malicious and malicious events 
dat.class0 <- MLData2023_cleaned %>% filter(Class == 0) # non-malicious 
dat.class1 <- MLData2023_cleaned %>% filter(Class == 1) # malicious 

# Randomly select 19800 non-malicious and 200 malicious samples, then combine them to form the training samples 
set.seed(10550239)
rows.train0 <- sample(1:nrow(dat.class0), size = 19800, replace = FALSE) 
rows.train1 <- sample(1:nrow(dat.class1), size = 200, replace = FALSE) 

# Your 20000 unbalanced training samples
train.class0 <- dat.class0[rows.train0,] # Non-malicious samples 
train.class1 <- dat.class1[rows.train1,] # Malicious samples 
mydata.ub.train <- rbind(train.class0, train.class1) 
mydata.ub.train <- mydata.ub.train %>% 
                                    mutate(Class = factor(Class, labels = c("NonMal","Mal")))

# Your 39600 balanced training samples, i.e. 19800 non-malicious and malicious samples each.
set.seed(10550239)
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

# Randomly selected two supervised learning modelling algorithms to test against one another
set.seed(10550239) 
models.list1 <- c("Logistic Ridge Regression", "Logistic LASSO Regression", "Logistic Elastic-Net Regression") 
models.list2 <- c("Classification Tree", "Bagging Tree", "Random Forest") 
myModels <- c(sample(models.list1, size = 1), sample(models.list2, size = 1)) 
myModels %>% data.frame

# Remove rows with missing values
mydata.ub.train_cleaned <- na.omit(mydata.ub.train)
mydata.b.train_cleaned <- na.omit(mydata.b.train)
sum(is.na(mydata.ub.train_cleaned))   # check missing values

# Separate predictor and outcome variables
predictors <- colnames(mydata.ub.train_cleaned)[!colnames(mydata.ub.train_cleaned) %in% c("Class")]
predictors1 <- colnames(mydata.b.train_cleaned)[!colnames(mydata.b.train_cleaned) %in% c("Class")]
outcome <- "Class"

# Check the class of each column in the dataset
# sapply(mydata.ub.train_cleaned, class)

# Define the character columns to be converted to numeric
char_cols <- c("Assembled.Payload.Size", "DYNRiskA.Score", "Response.Size", "Source.Ping.Time", "Connection.Rate", "Server.Response.Packet.Time", "Packet.Size", "Packet.TTL", "Source.IP.Concurrent.Connection")

# Convert character columns to numeric
mydata.ub.train_cleaned[, char_cols] <- apply(mydata.ub.train_cleaned[, char_cols], 2, function(x) {
    if (is.character(x)) {
    as.numeric(x)
    } else {
    x
    }
})

# # Subset the data frame to only use the specified columns
# mydata.ub.train_cleaned <- mydata.ub.train_cleaned[, char_cols]

# Train logistic elastic-net regression model on unbalanced dataset
model1_ub <- cv.glmnet(as.matrix(mydata.ub.train_cleaned[,predictors]), 
                        as.factor(mydata.ub.train_cleaned[,outcome]), 
                        alpha = 0.5, family = "binomial", nfolds = 5)

# Train random forest model on unbalanced dataset
model2_ub <- randomForest(as.factor(mydata.ub.train_cleaned[,outcome]) ~ ., 
                            data=mydata.ub.train_cleaned[,predictors], 
                            ntree=500, mtry=3, na.action = "na.pass")

# Define the character columns to be converted to numeric
char_cols1 <- c("Assembled.Payload.Size", "DYNRiskA.Score", "Response.Size", "Source.Ping.Time", "Connection.Rate", "Server.Response.Packet.Time", "Packet.Size", "Packet.TTL", "Source.IP.Concurrent.Connection")

# Convert character columns to numeric
mydata.b.train_cleaned[, char_cols1] <- apply(mydata.b.train_cleaned[, char_cols1], 2, function(x) {
    if (is.character(x)) {
    as.numeric(x)
    } else {
    x
    }
})

# # Subset the data frame to only use the specified columns
# mydata.b.train_cleaned <- mydata.b.train_cleaned[, char_cols1]

# Train logistic elastic-net regression model on balanced dataset
model1_b <- cv.glmnet(as.matrix(mydata.b.train_cleaned[,predictors1]), 
                        as.factor(mydata.b.train_cleaned[,outcome]), 
                        alpha = 0.5, family = "binomial", nfolds = 5)

# Train random forest model on balanced dataset
model2_b <- randomForest(as.factor(mydata.b.train_cleaned[,outcome]) ~ ., 
                        data=mydata.b.train_cleaned[,predictors1], 
                        ntree=500, mtry=3, na.action = "na.pass")

# Set up k-fold cross-validation with repeated measures
k <- 10
n_repeats <- 3
mycontrol <- trainControl(method = "repeatedcv", number = k, repeats = n_repeats, classProbs = TRUE, summaryFunction = twoClassSummary)

# Tune logistic elastic-net regression model
grid1 <- expand.grid(alpha=seq(0,1,0.1), lambda=seq(0,1,0.1))
set.seed(10550239)
model1_ub_tuned <- train(as.matrix(mydata.ub.train_cleaned[,predictors]), as.factor(mydata.ub.train_cleaned[,outcome]), method="glmnet", trControl=mycontrol, tuneGrid=grid1)

grid1 <- expand.grid(alpha=seq(0,1,0.1), lambda=seq(0,1,0.1))
set.seed(10550239)
model1_b_tuned <- train(as.matrix(mydata.b.train_cleaned[,predictors]), as.factor(mydata.b.train_cleaned[,outcome]), method="glmnet", trControl=mycontrol, tuneGrid=grid1)

# Tune random forest model
grid2 <- expand.grid(ntree=c(100, 250, 500), mtry=c(2, 3, 4))
set.seed(10550239)
model2_ub_tuned <- train(as.factor(mydata.ub.train_cleaned[,outcome]) ~ ., data=mydata.ub.train_cleaned[,predictors], method="rf", trControl=mycontrol, tuneGrid=grid2)

grid2 <- expand.grid(ntree=c(100, 250, 500), mtry=c(2, 3, 4))
set.seed(10550239)
model2_b_tuned <- train(as.factor(mydata.b.train_cleaned[,outcome]) ~ ., data=mydata.b.train_cleaned[,predictors], method="rf", trControl=mycontrol, tuneGrid=grid2)

# Generate predicted probabilities for logistic elastic-net regression model on unbalanced dataset
pred1_ub <- predict(model1_ub, as.matrix(mydata.test[,predictors]), type="response")
# Calculate AUC for logistic elastic-net regression model on unbalanced dataset
auc1_ub <- roc(mydata.test[,outcome], pred1_ub)$auc

# Generate predicted probabilities for random forest model on unbalanced dataset
pred2_ub <- predict(model2_ub, newdata=mydata.test[,predictors], type="prob")[,2]
# Calculate AUC for random forest model on unbalanced dataset
auc2_ub <- roc(mydata.test[,outcome], pred2_ub)$auc

# Generate predicted probabilities for logistic elastic-net regression model on balanced dataset
pred1_b <- predict(model1_b, as.matrix(mydata.test[,predictors]), type="response")
# Calculate AUC for logistic elastic-net regression model on balanced dataset
auc1_b <- roc(mydata.test[,outcome], pred1_b)$auc

# Generate predicted probabilities for random forest model on balanced dataset
pred2_b <- predict(model2_b, newdata=mydata.test[,predictors], type="prob")[,2]
# Calculate AUC for random forest model on balanced dataset
auc2_b <- roc(mydata.test[,outcome], pred2_b)$auc

# Make predictions on the testing set using the two models trained on the unbalanced and balanced datasets
pred1_ub <- predict(model1_ub, newx = as.matrix(mydata.test[,predictors]), type="response")
pred2_ub <- predict(model2_ub, newdata = mydata.test[,predictors], type="response")
pred1_b <- predict(model1_b, newx = as.matrix(mydata.test[,predictors]), type="response")
pred2_b <- predict(model2_b, newdata = mydata.test[,predictors], type="response")

# Create confusion matrices
cm1_ub <- table(mydata.test[,outcome], pred1_ub > 0.5)
cm2_ub <- table(mydata.test[,outcome], pred2_ub)
cm1_b <- table(mydata.test[,outcome], pred1_b > 0.5)
cm2_b <- table(mydata.test[,outcome], pred2_b)

# Calculate performance metrics
FP1_ub <- cm1_ub[1,2] / sum(cm1_ub[1,])
FN1_ub <- cm1_ub[2,1] / sum(cm1_ub[2,])
Acc1_ub <- sum(diag(cm1_ub)) / sum(cm1_ub)
Prec1_ub <- cm1_ub[2,2] / sum(cm1_ub[,2])
Rec1_ub <- cm1_ub[2,2] / sum(cm1_ub[2,])
F1_ub <- 2 * (Prec1_ub * Rec1_ub) / (Prec1_ub + Rec1_ub)
FP2_ub <- cm2_ub[1,2] / sum(cm2_ub[1,])
FN2_ub <- cm2_ub[2,1] / sum(cm2_ub[2,])
Acc2_ub <- sum(diag(cm2_ub)) / sum(cm2_ub)
Prec2_ub <- cm2_ub[2,2] / sum(cm2_ub[,2])
Rec2_ub <- cm2_ub[2,2] / sum(cm2_ub[2,])
F2_ub <- 2 * (Prec2_ub * Rec2_ub) / (Prec2_ub + Rec2_ub)
FP1_b <- cm1_b[1,2] / sum(cm1_b[1,])
FN1_b <- cm1_b[2,1] / sum(cm1_b[2,])
Acc1_b <- sum(diag(cm1_b)) / sum(cm1_b)
Prec1_b <- cm1_b[2,2] / sum(cm1_b[,2])
Rec1_b <- cm1_b[2,2] / sum(cm1_b[2,])
F1_b <- 2 * (Prec1_b * Rec1_b) / (Prec1_b + Rec1_b)
FP2_b <- cm2_b[1,2] / sum(cm2_b[1,])
FN2_b <- cm2_b[2,1] / sum(cm2_b[2,])
Acc2_b <- sum(diag(cm2_b)) / sum(cm2_b)
Prec2_b <- cm2_b[2,2] / sum(cm2_b[,2])
Rec2_b <- cm2_b[2,2] / sum(cm2_b[2,])
F2_b <- 2 * (Prec2_b * Rec2_b) / (Prec2_b + Rec2_b)

# Print results
cat("\nModel 1 (unbalanced training set):\n")
cat("False positive rate:", FP1_ub, "\n")
cat("False negative rate:", FN1_ub, "\n")
cat("Accuracy:", Acc1_ub, "\n")
cat("Precision:", Prec1_ub, "\n")
cat("Recall:", Rec1_ub, "\n")
cat("F1 score:", F1_ub, "\n\n")

cat("Model 2 (balanced training set):\n")
cat("False positive rate:", FP2_b, "\n")
cat("False negative rate:", FN2_b, "\n")
cat("Accuracy:", Acc2_b, "\n")
cat("Precision:", Prec2_b, "\n")
cat("Recall:", Rec2_b, "\n")
cat("F1 score:", F2_b, "\n\n")

