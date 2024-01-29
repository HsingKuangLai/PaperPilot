library(sandwich)
library(lmtest)
library(MASS)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("data.csv", na.strings = "#N/A")

# 移除含有缺失值的觀測值
data <- na.omit(data)
data$RPA_Ctd <- factor(data$RPA_Ctd)
#data$RPA <- factor(data$RPA)
data$GC <- factor(data$GC)
data$Big4 <- factor(data$Big4)
data$Industry <- factor(data$Industry)
#data$Year <- factor(data$Year)
data$Finance <- factor(data$Finance)
data$ABSDA<-as.numeric(data$ABSDA)
data$ABSDA_ROA<-abs(as.numeric(data$DA_ROA))
data$RAM<-data$ABCFO+data$ABEXP
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

library(MatchIt)
# Split the data table based on the condition (Price > 0)
#group1 <- subset(data, data$DA > 0)

# 假設你的資料框架名為 data
ps_model <- glm(RPA ~ (ADJROA + LGTA + Age + RD + ESG + GC + Big4) + Year, data = data)

summary(ps_model)

# 提取傾向分數
data$propensity_score <- predict(ps_model)

# 執行傾向分數匹配
matched_data<- matchit( RPA ~ (ABSDA_ROA + LEV + OCF + MTB  + ADJROA + LGTA + Age + RD + ESG + GC + Big4) + Year , data = data, method = "nearest",distance = "glm")
data <- match.data(matched_data)

#Winsorize
data$DA<-winsorize(data$DA)
data$DA_ROA<-winsorize(data$DA_ROA)
data$ABSDA_ROA<-winsorize(data$ABSDA_ROA)
data$ABSDA<-winsorize(data$ABSDA)
data$ABCFO<-winsorize(data$ABCFO)
data$ABPROD<-winsorize(data$ABPROD)
data$ABEXP<-winsorize(data$ABEXP)
data$RAM<-winsorize(data$RAM)
data$LGTA<-winsorize(data$LGTA)
data$LEV<-winsorize(data$LEV)
data$OCF<-winsorize(data$OCF)
data$MTB<-winsorize(data$MTB)
data$ESG<-winsorize(data$ESG)
data$Age<-winsorize(data$Age)
data$ROA<-winsorize(data$ROA)
data$ADJROA<-winsorize(data$ADJROA)
data$Age<-log(1+winsorize(data$Age))
data$Age_Trade<-log(1+winsorize(data$Age_Trade))
data$RPA_Count<-winsorize(data$RPA_Count)

###
sink("PSM_RM.txt")
model <- (lm((RAM) ~ RPA  + (ABSDA_ROA + LEV + OCF + MTB  + ADJROA + LGTA + Age + RD + ESG + Big4) + Year   , data = data))
summary(model)
coeftest(model, vcov = vcovHC(model))
coeftest(model, vcov = vcovCL(model,cluster = ~Key))
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



