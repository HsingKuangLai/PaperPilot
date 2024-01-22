# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("data.csv", na.strings = "#N/A")

# 移除含有缺失值的觀測值data$RPA <- factor(data$RPA)
data <- na.omit(data)
data$RPA <- factor(data$RPA)
data$Year <- factor(data$Year)
data$Industry <- factor(data$Industry)
data$Finance <- factor(data$Finance)
data$dvps1<-as.numeric(data$dvps1)
data <- na.omit(data)

data<-subset(data,data$Finance==0)

############################### winsorizing 1% greater (But equal to dummy)

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

data$Price<-winsorize(data$Price)
data$eps1<-winsorize(data$eps1)
data$dvps1<-winsorize(data$dvps1)
data$eps2<-winsorize(data$eps2)
data$LGTA<-log(winsorize(data$Asset))

#1 Remove BVE and LN(MVE)
#2 Remain, + Year + Industry
#Now, perform the Huber regression or any regression analysis using winsorized variables + ADV + RD  + ROA + S + SG + BM
model <- (lm((Price) ~ RPA + ( eps1 + eps2 + dvps1  + LGTA + Industry + Year)  , data = data))
summary(model)


# 將結果儲存到文字檔案cat(summary_text, sep = "\n")
summary_text <- capture.output(summary(model))
sink("Muhanna_summary_0112_FixHetero.txt")
summary(model)
coeftest(model, vcov = vcovHC(model, type="HC3"))
sink()

############################Assumptions
library("lmtest")
##Normality of Residuals
# Generate a QQ plot for residuals
qqnorm(residuals(model))
qqline(residuals(model), col = 2)  # Add a reference line
residuals <- residuals(model)
ks_test_result <- ks.test(residuals, "pnorm")
print(ks_test_result)

#Homoscedasticity (Constant Variance of Residuals)
# Plot square root of absolute residuals vs. fitted values

sqrt_abs_resid <- sqrt(abs(residuals(model)))
plot(model$fitted.values, sqrt_abs_resid,
     xlab = "Fitted Values", ylab = "Square Root of Absolute Residuals",
     main = "Scale-Location Plot")
abline(h = mean(sqrt_abs_resid), col = "red", lty = 2)  # Add a horizontal line at mean
bptest(model)

## Use White 
library(sandwich)
coeftest(model, vcov = vcovHC(model, type="HC3"))

#No Autocorrelation of Residuals
bgtest(model)

#Corr
# Calculate the correlation matrix
data$RPA<-as.numeric(data$RPA)
cor_matrix <- cor(data[, c("MVE", "RPA", "Earnings", "NetDiv", "ADV", "RD", "ROA", "S", "SG", "BM")])
# Print the correlation matrix
print(cor_matrix)




