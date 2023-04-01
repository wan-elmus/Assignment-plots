


# Load required packages
library(dplyr)
library(forcats)
library(glmnet)
library(randomForest)

# import data
dat <- read.csv("MLData2023.csv", stringsAsFactors = TRUE)

# Clean the whole dataset
dat <- dat %>%


mutate(IPV6.Traffic = if_else(IPV6.Traffic == "Yes", 1, 0), # Convert Yes/No to 1/0 for binary features
         Ingress.Router= if_else(Ingress.Router == "Yes", 1, 0),
         Assembled.Payload.Size = if_else(Assembled.Payload.Size == "Yes", 1, 0),
         DYNRiskA.Score = if_else(DYNRiskA.Score== "Yes", 1, 0),
         Response.Size = if_else(Response.Size == "Yes", 1, 0),
         Source.Ping.Time = if_else(Source.Ping.Time == "Yes", 1, 0),
         Connection.State = if_else(Connection.State == "Yes", 1, 0),
         Server.Response.Packet.Time = if_else(Server.Response.Packet.Time == "Yes", 1, 0),
         Packet.Size = if_else(Packet.Size == "Yes", 1, 0),
         Packet.TTL = if_else(Packet.TTL == "Yes", 1, 0),
         Source.IP.Concurrent.Connection = if_else(Source.IP.Concurrent.Connection == "Yes", 1, 0),
         Operating.System = if_else(Operating.System == "Yes", 1, 0))


# Filter the data to only include cases labelled with Class = 0 or 1
dat <- dat %>%
  filter(Class == 0 | Class == 1)

  # Convert dat$Operating.System to a factor if needed
if(!is.factor(dat$Operating.System)) {
  dat$Operating.System <- as.factor(dat$Operating.System)
}

# Merge the three Windows categories together to form a new category named Windows_All and merge iOS, Linux (Unknown), and Other to form the new category named Others for the feature Operating.System
dat$Operating.System <- fct_collapse(dat$Operating.System, Windows_All = c("Windows 7", "Windows 8", "Windows 10"), Others = c("iOS", "Linux (Unknown)", "Other"))

# Merge INVALID, NEW and RELATED to form the new category named Others for the feature Connection.State
dat$Connection.State <- fct_collapse(dat$Connection.State, Others = c("INVALID", "NEW", "RELATED"))

# Select only the relevant features for modeling
dat <- dat %>%
dat <- dat %>%

select (Assembled.Payload.Size, DYNRiskA.Score, Response.Size, Source.Ping.Time, Connection.State, Server.Response.Packet.Time, Packet.Size, Packet.TTL, Source.IP.Concurrent.Connection, Class, Operating.System) %>%
# select(IPV6.Traffic, Operating.System, Connection.State, Ingress.Router) %>%

# remove rows with missing values
#   na.omit()
dat <- dat %>% na.omit()

# save cleaned data
write.csv(dat, "MLData2023_cleaned.csv", row.names = FALSE)

# Print the first few rows of the cleaned and filtered dataset
head(dat)

# Separate samples of non-malicious and malicious events 
dat.class0 <- dat %>% filter(Class == 0) # non-malicious 
dat.class1 <- dat %>% filter(Class == 1) # malicious 
# Randomly select 19800 non-malicious and 200 malicious samples, then combine them to form the training samples 
set.seed(Enter your student ID) 
rows.train0 <- sample(1:nrow(dat.class0), size = 19800, replace = FALSE) 
rows.train1 <- sample(1:nrow(dat.class1), size = 200, replace = FALSE) 
# Your 20000 unbalanced training samples 
train.class0 <- dat.class0[rows.train0,] # Non-malicious samples 
train.class1 <- dat.class1[rows.train1,] # Malicious samples 
mydata.ub.train <- rbind(train.class0, train.class1) 
mydata.ub.train <- mydata.ub.train %>% 
                                     mutate(Class = factor(Class, labels = c("NonMal","Mal")))
# Your 39600 balanced training samples, i.e. 19800 non-malicious and malicious sam ples each. set.seed(123) 
train.class1_2 <- train.class1[sample(1:nrow(train.class1), size = 19800, 
                                 replace = TRUE),] 
mydata.b.train <- rbind(train.class0, train.class1_2) 
mydata.b.train <- mydata.b.train %>%         
                                     mutate(Class = factor(Class, labels = c("NonMal","Mal")))
# Your testing samples 
test.class0 <- dat.class0[-rows.train0,] 
test.class1 <- dat.class1[-rows.train1,] 
mydata.test <- rbind(test.class0, test.class1) 
mydata.test <- mydata.test %>% 
                             mutate(Class = factor(Class, labels = c("NonMal","Mal")))



set.seed(Enter Student ID)
models.list1 <- c("Logistic Ridge Regression", "Logistic LASSO Regression", "Logistic Elastic-Net Regression") 
models.list2 <- c("Classification Tree", "Bagging Tree", "Random Forest") 
myModels <- c(sample(models.list1, size = 1), sample(models.list2, size = 1)) 
myModels %>% data.frame












# mutate(IPV6.Traffic = if_else(IPV6.Traffic == "Yes", 1, 0), # Convert Yes/No to 1/0 for binary features
#          Ingress.Router= if_else(Ingress.Router == "Yes", 1, 0),
#          Assembled.Payload.Size = if_else(Assembled.Payload.Size == "Yes", 1, 0),
#          DYNRiskA.Score = if_else(DYNRiskA.Score== "Yes", 1, 0),
#          Response.Size = if_else(Response.Size == "Yes", 1, 0),
#          Source.Ping.Time = if_else(Source.Ping.Time == "Yes", 1, 0),
#          Connection.State = if_else(Connection.State == "Yes", 1, 0)
#          Server.Response.Packet.Time = if_else(Server.Response.Packet.Time == "Yes", 1, 0)
#          Packet.Size = if_else(Packet.Size == "Yes", 1, 0)
#          Packet.TTL = if_else(Packet.TTL == "Yes", 1, 0)
#          Source.IP.Concurrent.Connection = if_else(Source.IP.Concurrent.Connection == "Yes", 1, 0))






         # select(IPV6.Traffic, Is.Encrypted, Is.Firewalled, Is.Malware, Is.Potentially.Unwanted, Is.Riskware, Is.Spyware, Connection.State, Operating.System, Class) %>%
# select ("Assembled.Payload.Size", "DYNRiskA.Score", "Response.Size", "Source.Ping.Time", "Connection.State", "Server.Response.Packet.Time", "Packet.Size", "Packet.TTL", "Source.IP.Concurrent.Connection", "Class") %>%