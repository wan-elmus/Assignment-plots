# You may need to change/include the path of your working directory

library(dplyr)
library(e1071)
library(ggplot2)

dat=read.csv("MLData2023.csv", stringsAsFactors = TRUE)

# Separate samples of non-malicious and malicious events
dat.class0 <- dat %>% filter(Class == 0) # non-malicious
dat.class1 <- dat %>% filter(Class == 1) # malicious

# Randomly select 300 samples from each class, then combine them to form a working dataset
set.seed(10550239)
rand.class0 <- dat.class0[sample(1:nrow(dat.class0), size = 300, replace = FALSE),]
rand.class1 <- dat.class1[sample(1:nrow(dat.class1), size = 300, replace = FALSE),]

# sub-sample of 600 observations
mydata <- rbind(rand.class0, rand.class1)
dim(mydata) # Check the dimension of your sub-sample

str(mydata)

cat_features=c("IPV6.Traffic", "Operating.System", "Connection.State", "Ingress.Router")
for (feature in cat_features) {
  cat_table <- table(mydata[[feature]], useNA = "ifany")
  cat_table <- cat_table[order(-cat_table)] # sort the table in descending order
  cat_perc <- prop.table(cat_table) * 100
  cat_summary <- data.frame(
    Category = names(cat_table),
    N = as.numeric(cat_table),
    Percent = paste0(round(cat_perc), "%"))
  rownames(cat_summary) <- NULL
  cat("Categorical Feature:", feature, "\n")
  print(cat_summary)
  cat("\n")
  flush.console()
}


num_features <- c("Assembled.Payload.Size", "DYNRiskA.Score", "Response.Size", "Source.Ping.Time", "Connection.State", "Server.Response.Packet.Time", "Packet.Size", "Packet.TTL", "Source.IP.Concurrent.Connection", "Class")


for (feature in num_features) {
  if (is.numeric(mydata[[feature]])) {
    n <- length(mydata[[feature]])
    missing <- sum(is.na(mydata[[feature]]))
    min_val <- min(mydata[[feature]], na.rm = TRUE)
    max_val <- max(mydata[[feature]], na.rm = TRUE)
    mean_val <- mean(mydata[[feature]], na.rm = TRUE)
    median_val <- median(mydata[[feature]], na.rm = TRUE)
    skewness_val <- e1071::skewness(mydata[[feature]], na.rm = TRUE)
    
    summary_table <- data.frame(
      Feature = feature,
      `Number (%)` = paste0(n, " (100.0%)"),
      Missing = paste0(missing, " (", round(missing/n*100, 1), "%)"),
      Min = ifelse(is.infinite(min_val), "NA", sprintf("%.2f", min_val)),
      Max = ifelse(is.infinite(max_val), "NA", sprintf("%.2f", max_val)),
      Mean = ifelse(is.na(mean_val), "NA", sprintf("%.2f", mean_val)),
      Median = ifelse(is.na(median_val), "NA", sprintf("%.2f", median_val)),
      Skewness = ifelse(is.na(skewness_val), "NA", sprintf("%.2f", skewness_val))
    )
    
    print(summary_table)
    cat("\n")
    flush.console()
  }
}

# Converting the columns to numeric
mydata$Assembled.Payload.Size <- as.numeric(mydata$Assembled.Payload.Size)
mydata$DYNRiskA.Score <- as.numeric(mydata$DYNRiskA.Score)
mydata$Response.Size <- as.numeric(mydata$Response.Size)
mydata$Source.Ping.Time <- as.numeric(mydata$Source.Ping.Time)
mydata$Connection.State <- as.numeric(mydata$Connection.State)
mydata$Server.Response.Packet.Time <- as.numeric(mydata$Server.Response.Packet.Time)
mydata$Packet.Size <- as.numeric(mydata$Packet.Size)
mydata$Packet.TTL <- as.numeric(mydata$Packet.TTL)
mydata$Source.IP.Concurrent.Connection <- as.numeric(mydata$Source.IP.Concurrent.Connection)
mydata$Class <- as.numeric(mydata$Class)

# creating a function to identify outliers and replace them with NAs
replace_outliers <- function(x, multiplier=1.5) {
  if(all(is.na(x))) {
    return(x)
  }
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  upper <- q3 + multiplier * iqr
  lower <- q1 - multiplier * iqr
  x[x < lower | x > upper] <- NA
  H <- 1.5 * iqr
  x[x < (q1 - H)] <- NA
  x[x > (q3 + H)] <- NA
  x[is.na(x)] <- median(x, na.rm = TRUE)
  
  return(x)
}

outlier_check <- function(x, multiplier=1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  upper <- q3 + multiplier * iqr
  lower <- q1 - multiplier * iqr
  outliers <- x < lower | x > upper
  return(outliers)
}

# outliers <- outlier_check(mydata$DYNRiskA.Score)
# sum(outliers)


pca <- prcomp(mydata[num_features], center = TRUE, scale. = TRUE)

loadings <- pca$rotation
scores <- pca$x


ggplot(data = data.frame(scores[,1:2], Class = as.factor(mydata$Class))) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1, margin = margin(t = 0, r = 5, b = 0, l = 5)))+
  geom_point(aes(x = PC1, y = PC2, col = Class), size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_text(data = as.data.frame(loadings), aes(x = PC1, y = PC2, label = rownames(loadings)), color = "blue", size = 3.5) + 
  geom_segment(data = as.data.frame(loadings), aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm")), color = "blue") + 
  theme_bw() + 
  xlab(paste0("PC1 (", round(pca$sdev[1]/sum(pca$sdev)*100, 1), "%)")) + 
  ylab(paste0("PC2 (", round(pca$sdev[2]/sum(pca$sdev)*100, 1), "%)")) + 
  ggtitle("Biplot of PCA Loadings and Scores")

if (sum(!is.na(mydata$DYNRiskA.Score)) > 0) {
mydata$DYNRiskA.Score <- replace_outliers(mydata$DYNRiskA.Score, multiplier = 1.5)
}

# apply the function to each continuous variable

mydata$Assembled.Payload.Size <- replace_outliers(mydata$Assembled.Payload.Size)
mydata$DYNRiskA.Score <- replace_outliers(mydata$DYNRiskA.Score)
mydata$Response.Size <- replace_outliers(mydata$Response.Size)
mydata$Source.Ping.Time <- replace_outliers(mydata$Source.Ping.Time)
mydata$Connection.State <- replace_outliers(mydata$Connection.State)
mydata$Server.Response.Packet.Time <- replace_outliers(mydata$Server.Response.Packet.Time)
mydata$Packet.Size=replace_outliers(mydata$Packet.Size)
mydata$Packet.TTL=replace_outliers(mydata$Packet.TTL)
mydata$Source.IP.Concurrent.Connection=replace_outliers(mydata$Source.IP.Concurrent.Connection)
mydata$Class=replace_outliers(mydata$Class)
