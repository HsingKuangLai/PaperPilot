# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("data.csv", na.strings = "#N/A")

# 移除含有缺失值的觀測值
data <- na.omit(data)
data$RPA_Ctd <- factor(data$RPA_Ctd)
data$RPA <- factor(data$RPA)
data$Industry <- factor(data$Industry)
#data$Year <- factor(data$Year)
data$Finance <- factor(data$Finance)
data$ABSDA<-as.numeric(data$ABSDA)
data$OCF<-as.numeric(data$OCF)
data <- na.omit(data)

############################### winsorizing 1% greater (But equal to dummy)

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}


# Split the data table based on the condition (Price > 0)
group1 <- data

group1$DA<-winsorize(group1$DA)
group1$ABSDA<-winsorize(group1$ABSDA)
group1$LGTA<-winsorize(group1$LGTA)
group1$LEV<-winsorize(group1$LEV)
group1$OCF<-winsorize(group1$OCF)
group1$MTB<-winsorize(group1$MTB)
group1$RPA_Count<-winsorize(group1$RPA_Count)

###
model <- (lm( DA ~ RPA + LGTA + LEV + OCF + MTB + Year  , data = group1))
summary(model)

##
library(MASS)
library(lmtest)
library(sandwich)
coeftest(model, vcov = vcovHC(model, type="HC3"))


# 將結果儲存到文字檔案
summary_text <- capture.output(summary(model))
sink("PDA.txt")
cat(summary_text, sep = "\n")
sink()

############################Assumptions
library("lmtest")
##Normality of Residuals
residuals <- residuals(model)
ks_test_result <- ks.test(residuals, "pnorm")
print(ks_test_result)

#Homoscedasticity (Constant Variance of Residuals)
bptest(model)

#No Autocorrelation of Residuals
bgtest(model)

#Corr
data$RPA<-as.numeric(data$RPA)
cor_matrix <- cor(data[, c("RPA","MVE","BVE", "Earnings", "NetDiv", "RD", "S", "SG", "BM")])
print(cor_matrix)

# Generate a QQ plot for residuals
qqnorm(residuals(model))
qqline(residuals(model), col = 2)  # Add a reference line



