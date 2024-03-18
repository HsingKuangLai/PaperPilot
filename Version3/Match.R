library(sandwich)
library(lmtest)
library(MASS)
library(dplyr)
library(stargazer)
library(multcomp)
library(MatchIt)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("total.csv")
data<-subset(data,data$Code_Count==6)

# 移除含有缺失值的觀測值
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABEXP<-data$ABEXP*(-1)
data$RM<-data$ABEXP+data$ABPROD
data$LGTA<-log(data$Asset)

########### winsorize 1% 
# Define a function for winsorize
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}


# Factorize the first 6 columns
#data[1:10] <- lapply(data[1:10], factor)

# Apply the winsorize function to the remaining columns
#data <- data %>%
  #mutate(across(.cols = 11:ncol(data), .fns = ~winsorize(.)))

data$ADJROA_sq<-data$ADJROA*data$ADJROA
data$YEAR<-(as.numeric(data$YEAR)+2016)

# Note: Adjust the column name 'TotalAssets' if different in your dataset
data <- data %>%
  group_by(Code) %>%
  mutate(AvgTotalAsset = mean(LGTA)) %>%
  ungroup()

data<-unique(data[,c("Code","Industry","AvgTotalAsset","RPA")])

# Separating treatment and control groups
treatment <- data %>% filter(RPA == 1)
control <- data %>% filter(RPA == 0)

# Matching based on Mahalanobis distance
# Note: Adjust 'Industry' and 'AvgTotalAssets' if column names differ
match_data <- matchit(RPA ~ Industry  + AvgTotalAsset, data = data, method = "nearest", distance = "mahalanobis")

# Getting the matched dataset
matched <- match.data(match_data)

# Optional: Selecting matched pairs for analysis
# This will depend on how you want to analyze the matched pairs
matched_pairs <- matched %>% 
  select(Code, Industry, AvgTotalAsset, RPA)

# Extract the subset of matched treatment and control observations
matched <- match.data(match_data)

# Generate a unique PairedCode for each matched pair
matched$PairedCode <- as.factor(matched$weights)

# Alternatively, if you need a numeric or more customized code
matched$PairedCode <- seq_len(nrow(matched))
