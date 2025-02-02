library(sandwich)
library(lmtest)
library(MASS)
library(dplyr)
library(stargazer)
library(corrtable)
library(coin)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("Ctd_Control.csv")

# 移除含有缺失值的觀測值
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABSDA2<-abs(data$DA2)
data$ABCFO<-data$ABCFO*(-1)
data$ABEXP<-data$ABEXP*(-1)
data$RM<-data$ABCFO+data$ABPROD+data$ABEXP
data$RM1<-data$ABCFO+data$ABEXP
data$RM2<-data$ABPROD+data$ABEXP


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
data[1:10] <- lapply(data[1:10], factor)

# Apply the winsorize function to the remaining columns
data <- data %>%
  mutate(across(.cols = 11:ncol(data), .fns = ~winsorize(.)))



# Des1
mydata<-data[,c("ABSDA1","ABCFO","ABPROD","ABEXP","RM","ADJROA","INST","MS","Age","RD","ADV","ESG","MTB","OCF","LEV","LGTA")]
stargazer(mydata, type = "html", title="Descriptive statistics", digits=5, out="des1.html",summary.stat = c("mean","median","sd","min","max","p25","p75","n"))


#Des2
correlation.matrix <- correlation_matrix(mydata,type="spearman",use = "lower",show_significance = TRUE,digits = 3)
stargazer(correlation.matrix,type="html", title="Correlation Matrix",out="des2.html")


#Des2.1
mydata<-data[,c("ABSDA1","ABCFO","ABPROD","ABEXP","RM")]
correlation.matrix <- correlation_matrix(mydata,type="spearman",use = "lower",show_significance = TRUE,digits = 3)
stargazer(correlation.matrix,type="html", title="Correlation Matrix",out="des2_1.html")

#Des3
mydata<-data[,c("POST","RPA","POST_RPA","ABSDA","ABPROD","ABEXP","RM","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL","ADJROA","LGTA","BIG4","RD","ADV")]
# Initialize a list to store results

variables<-c("ABSDA","ABPROD","ABEXP","RM","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL","ADJROA","LGTA","BIG4","RD","ADV")

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
  test_result <- wilcox_test(reformulate("RPA", response = var), data = mydata)
  p_value <- pvalue(test_result)
  
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
stargazer(results_matrix, type = "html",out="des31.html",digits = 4, title = "Comparative Statistics: RPA vs. Non-RPA Firms", summary = FALSE)
