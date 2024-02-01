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
data$Key <- factor(data$Key)
data$Big4 <- factor(data$Big4)
data$Industry <- factor(data$Industry)
data$Year <- factor(data$Year)
data$Finance <- factor(data$Finance)
data$ABSDA<-as.numeric(data$ABSDA)
data$ADV<-as.numeric(data$ADV)
data$ABSDA_ROA<-abs(as.numeric(data$DA_ROA))
data$RAM<-data$ABCFO+data$ABEXP
data$Age<-log(1+(data$Age))
data$Age_Trade<-log(1+(data$Age_Trade))
data$OCF<-as.numeric(data$OCF)
data <- na.omit(data)

#data<-subset(data,data$DA<=0)
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

# Identify columns that are numeric and not factor
numeric_cols <- sapply(data, is.numeric) & !sapply(data, is.factor)

# Apply the winsorize function to the selected numeric columns
data[numeric_cols] <- lapply(data[numeric_cols], winsorize)
data$ADJROA_sq<-data$ADJROA*data$ADJROA


# Kim: ESG,RD,Big4, GC....(+ Big4 + GC + ESG + RD)
#Now, perform the Huber regression or any regression analysis using winsorized variables+ Year + Industry
#sink("Kim_ABS(sq).txt")
model <- (lm((ABSDA_ROA) ~ RPA  + (RAM + LEV + OCF + MTB  + ADJROA  + LGTA + Age + Big4 + RD + ADV + ESG + GC ) + Year  , data = data))
summary(model)
coeftest(model, vcov = vcovHC(model))
coeftest(model, vcov = vcovCL(model,cluster = ~Key))


model <- (lm((RAM) ~ RPA  + (ABSDA_ROA + LEV + OCF + MTB  + ADJROA + LGTA + Age + Big4 + RD + ADV + ESG + GC ) + Year   , data = data))
summary(model)
##, type="HC3"
coeftest(model, vcov = vcovHC(model))
coeftest(model, vcov = vcovCL(model,cluster = ~Key))
#sink()

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



