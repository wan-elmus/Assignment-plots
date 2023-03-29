



# Separate samples of non-malicious and malicious events 
dat.class0 <- MLData2023_cleaned %>% filter(Class == 0) # non-malicious 
dat.class1 <- MLData2023_cleaned %>% filter(Class == 1) # malicious 

# Randomly select 19800 non-malicious and 200 malicious samples, then combine them to form the training samples 
set.seed(980773) # Enter your student ID here 
rows.train0 <- sample(1:nrow(dat.class0), size = 19800, replace = FALSE) 
rows.train1 <- sample(1:nrow(dat.class1), size = 200, replace = FALSE) 

# Your 20000 unbalanced training samples 
train.class0 <- dat.class0[rows.train0,] # Non-malicious samples 
train.class1 <- dat.class1[rows.train1,] # Malicious samples 
mydata.ub.train <- rbind(train.class0, train.class1) 
mydata.ub.train <- mydata.ub.train %>% 
                                    mutate(Class = factor(Class, labels = c("NonMal","Mal")))

# Your 39600 balanced training samples, i.e. 19800 non-malicious and malicious samples each. 
set.seed(980773) # Enter your student ID here 
train.class1_2 <- train.class1[sample(1:nrow(train.class1), size = 19800, 
                                replace = TRUE),] 
mydata.b.train <- rbind(train.class0, train.class1_2) 
mydata.b.train <- mydata.b.train %>%         
                                    mutate(Class = factor(Class, labels
