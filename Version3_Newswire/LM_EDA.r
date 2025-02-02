library(sandwich)
library(lmtest)
library(MASS)
library(dplyr)
library(stargazer)
library(corrtable)
library(coin)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("total - 複製2.csv")

# 移除含有缺失值的觀測值
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABCFO<-data$ABCFO*(-1)
data$ABEXP<-data$ABEXP*(-1)
data$RM<-data$ABPROD+data$ABEXP
data$SIZE<-data$LGTA


########### winsorizing 1% 
# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

# Factorize the first 6 columns
data[1:12] <- lapply(data[1:12], factor)

# Apply the winsorize function to the remaining columns
data <- data %>%
  mutate(across(.cols = 13:ncol(data), .fns = ~winsorize(.)))



# Des1
mydata<-data[,c("POST","RPA","ABSDA","ABPROD","ABEXP","RM","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL","ADJROA","SIZE","BIG4","RD","ADV")]
mydata$BIG4<-as.numeric(data$BIG4)-1
mydata$RPA<-as.numeric(data$RPA)-1
mydata$POST<-as.numeric(data$POST)-1
stargazer(mydata, type = "html", title="Descriptive statistics", digits=5, out="Table2 PanelA.html",summary.stat = c("mean","median","sd","min","max","p25","p75","n"))


#Des2
correlation.matrix <- correlation_matrix(mydata,type="spearman",use = "lower",show_significance = TRUE,digits = 3)
stargazer(correlation.matrix,type="html", title="Correlation Matrix",out="Table2 PanelB.html")



#Des3A
mydata<-data[,c("POST","RPA","POST_RPA","ABSDA","ABPROD","ABEXP","RM","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL","ADJROA","SIZE","BIG4","RD","ADV")]
# Initialize a list to store results

variables<-c("ABSDA","ABPROD","ABEXP","RM","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL","ADJROA","SIZE","BIG4","RD","ADV")

results <- list()

mydata$BIG4<-as.numeric(mydata$BIG4)

# Loop through each variable to calculate stats and perform tests
for (var in variables) {
  # Separate the groups
  group_rpa <- filter(mydata, RPA == 1 & POST==0) %>% pull(!!sym(var))
  group_non_rpa <- filter(mydata, RPA == 1 & POST==1) %>% pull(!!sym(var))
  
  # Calculate statistics
  mean_rpa <- mean(group_rpa, na.rm = TRUE)
  median_rpa <- median(group_rpa, na.rm = TRUE)
  sd_rpa <- sd(group_rpa, na.rm = TRUE)
  
  mean_non_rpa <- mean(group_non_rpa, na.rm = TRUE)
  median_non_rpa <- median(group_non_rpa, na.rm = TRUE)
  sd_non_rpa <- sd(group_non_rpa, na.rm = TRUE)
  
  mydata<-subset(mydata,mydata$RPA==1)
  
  # Wilcoxon test
  test_result <- t.test(reformulate("POST", response = var), data = mydata)
  p_value <- (test_result)$p.value
  
  # Store results
  results[[var]] <- c(mean_rpa, sd_rpa, mean_non_rpa, sd_non_rpa, p_value)
}

# Convert results to a dataframe for easier manipulation and display
results_df <- do.call(rbind, results)
colnames(results_df) <- c("Mean", "SD", "Mean-Post", "SD-Post", "P-Value")
rownames(results_df) <- variables
# Convert the results dataframe to a matrix for stargazer
results_matrix <- as.matrix(results_df)

# Use stargazer to create a table
stargazer(results_matrix, type = "html",out="Table3 PanelA.html",digits = 4, title = "RPA Implementers’ Pre- versus Post-Implementation", summary = FALSE)


#Des3B
mydata<-data[,c("POST","RPA","POST_RPA","ABSDA","ABPROD","ABEXP","RM","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL","ADJROA","SIZE","BIG4","RD","ADV")]
# Initialize a list to store results

variables<-c("ABSDA","ABPROD","ABEXP","RM","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL","ADJROA","SIZE","BIG4","RD","ADV")

results <- list()

mydata$BIG4<-as.numeric(mydata$BIG4)

# Loop through each variable to calculate stats and perform tests
for (var in variables) {
  # Separate the groups
  group_rpa <- filter(mydata, RPA == 0 & POST==0) %>% pull(!!sym(var))
  group_non_rpa <- filter(mydata, RPA == 1 & POST==0) %>% pull(!!sym(var))
  
  # Calculate statistics
  mean_rpa <- mean(group_rpa, na.rm = TRUE)
  median_rpa <- median(group_rpa, na.rm = TRUE)
  sd_rpa <- sd(group_rpa, na.rm = TRUE)
  
  mean_non_rpa <- mean(group_non_rpa, na.rm = TRUE)
  median_non_rpa <- median(group_non_rpa, na.rm = TRUE)
  sd_non_rpa <- sd(group_non_rpa, na.rm = TRUE)
  
  mydata<-subset(mydata,mydata$POST==0)
  
  # Wilcoxon test
  test_result <- t.test(reformulate("RPA", response = var), data = mydata)
  p_value <- (test_result)$p.value
  
  # Store results
  results[[var]] <- c(mean_rpa, sd_rpa, mean_non_rpa, sd_non_rpa, p_value)
}

# Convert results to a dataframe for easier manipulation and display
results_df <- do.call(rbind, results)
colnames(results_df) <- c("Mean Control", "SD Control", "Mean Treatment", "SD Treatment", "P-Value")
rownames(results_df) <- variables
# Convert the results dataframe to a matrix for stargazer
results_matrix <- as.matrix(results_df)

# Use stargazer to create a table
stargazer(results_matrix, type = "html",out="Tabel3 PanelB.html",digits = 4, title = "RPA Implementers versus Control Group in the Pre-Implementation Period", summary = FALSE)


#Des3C
mydata<-data[,c("POST","RPA","POST_RPA","ABSDA","ABPROD","ABEXP","RM","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL","ADJROA","SIZE","BIG4","RD","ADV")]
# Initialize a list to store results

variables<-c("ABSDA","ABPROD","ABEXP","RM","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL","ADJROA","SIZE","BIG4","RD","ADV")

results <- list()

mydata$BIG4<-as.numeric(mydata$BIG4)

# Loop through each variable to calculate stats and perform tests
for (var in variables) {
  # Separate the groups
  group_rpa <- filter(mydata, RPA == 0 & POST==1) %>% pull(!!sym(var))
  group_non_rpa <- filter(mydata, RPA == 1 & POST==1) %>% pull(!!sym(var))
  
  # Calculate statistics
  mean_rpa <- mean(group_rpa, na.rm = TRUE)
  median_rpa <- median(group_rpa, na.rm = TRUE)
  sd_rpa <- sd(group_rpa, na.rm = TRUE)
  
  mean_non_rpa <- mean(group_non_rpa, na.rm = TRUE)
  median_non_rpa <- median(group_non_rpa, na.rm = TRUE)
  sd_non_rpa <- sd(group_non_rpa, na.rm = TRUE)
  
  mydata<-subset(mydata,mydata$POST==1)
  
  # Wilcoxon test
  test_result <- t.test(reformulate("RPA", response = var), data = mydata)
  p_value <- (test_result)$p.value
  
  # Store results
  results[[var]] <- c(mean_rpa, sd_rpa, mean_non_rpa, sd_non_rpa, p_value)
}

# Convert results to a dataframe for easier manipulation and display
results_df <- do.call(rbind, results)
colnames(results_df) <- c("Mean Control", "SD Control", "Mean Treatment", "SD Treatment", "P-Value")
rownames(results_df) <- variables
# Convert the results dataframe to a matrix for stargazer
results_matrix <- as.matrix(results_df)

# Use stargazer to create a table
stargazer(results_matrix, type = "html",out="Table3 PanelC.html",digits = 4, title = "RPA Implementers versus Control Group in the Post-Implementation Period", summary = FALSE)
