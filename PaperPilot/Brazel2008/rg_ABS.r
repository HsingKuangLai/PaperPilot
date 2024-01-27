library(sandwich)
library(lmtest)
library(MASS)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("data.csv", na.strings = "#N/A")

# 移除含有缺失值的觀測值
data <- na.omit(data)
data$RPA_Ctd <- factor(data$RPA_Ctd)
data$RPA <- factor(data$RPA)
data$GC <- factor(data$GC)
data$Big4 <- factor(data$Big4)
data$Industry <- factor(data$Industry)
#data$Year <- factor(data$Year)
data$Finance <- factor(data$Finance)
data$ABSDA<-as.numeric(data$ABSDA)
data$ABSDA_ROA<-abs(as.numeric(data$DA_ROA))
data$RAM<-data$ABCFO+data$ABEXP-data$ABPROD
data$OCF<-as.numeric(data$OCF)
data <- na.omit(data)

#data<-subset(data,data$DA>=0)
#data<-subset(data,data$Finance==0)

############################### winsorizing 1% greater (But equal to dummy)

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

data$DA<-winsorize(data$DA)
data$DA_ROA<-winsorize(data$DA_ROA)
data$ABSDA_ROA<-winsorize(data$ABSDA_ROA)
data$ABSDA<-winsorize(data$ABSDA)
#data$ABCFO<-winsorize(data$ABCFO)
data$ABPROD<-winsorize(data$ABPROD)
data$ABEXP<-winsorize(data$ABEXP)
data$RAM<-winsorize(data$RAM)
data$LGTA<-winsorize(data$LGTA)
data$LEV<-winsorize(data$LEV)
data$OCF<-winsorize(data$OCF)
data$MTB<-winsorize(data$MTB)
data$ESG<-winsorize(data$ESG)
data$Age<-winsorize(data$Age)
data$Age<-log(1+winsorize(data$Age))
data$Age_Trade<-log(1+winsorize(data$Age_Trade))
data$RPA_Count<-winsorize(data$RPA_Count)



# Kim: ESG,RD,Big4, GC....(+ Big4 + GC + ESG + RD)
#Now, perform the Huber regression or any regression analysis using winsorized variables+ Year + Industry
model <- (lm((ABCFO) ~ RPA  + (ABSDA + LEV + OCF + MTB + LGTA + Age_Trade + Big4 + GC + ESG + RD) + Year   , data = data))
summary(model)


##, type="HC3"
library(sandwich)
library(lmtest)
coeftest(model, vcov = vcovHC(model))
#coeftest(model, vcov = vcovCL(model,cluster = ~Key))


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


# Generate a QQ plot for residuals
qqnorm(residuals(model))
qqline(residuals(model), col = 2)  # Add a reference line



