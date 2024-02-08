library(sandwich)
library(lmtest)
library(MASS)
library(dplyr)
library(stargazer)
library(corrtable)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("Total.csv")

# 移除含有缺失值的觀測值
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABSDA2<-abs(data$DA2)
data$ABSDA3<-abs(data$DA3)
data$RM<-data$ABCFO+data$ABEXP-data$ABPROD

#data<-subset(data,data$DA2<0)

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
data[1:8] <- lapply(data[1:8], factor)

# Apply the winsorize function to the remaining columns
data <- data %>%
  mutate(across(.cols = 9:ncol(data), .fns = ~winsorize(.)))

# Des1
mydata<-data[,c("ABSDA2","DA2","ABCFO","ABPROD","ABEXP","RM","ADJROA","Age","RD","ADV","ESG","MTB","OCF","LEV","LGTA")]
stargazer(mydata, type = "html", title="Descriptive statistics", digits=5, out="des1.html",summary.stat = c("mean","median","sd","min","max","p25","p75","n"))


#Des2
correlation.matrix <- correlation_matrix(mydata,type="spearman",use = "lower",show_significance = TRUE,digits = 3)
stargazer(correlation.matrix,type="html", title="Correlation Matrix",out="des2.html")

