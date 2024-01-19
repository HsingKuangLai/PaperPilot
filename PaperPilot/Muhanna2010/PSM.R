# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("data.csv", na.strings = "#N/A")

# 移除含有缺失值的觀測值
data <- na.omit(data)
data$Year <- factor(data$Year)
data$Industry <- factor(data$Industry)
data$Finance <- factor(data$Finance)
data$NetDiv<-as.numeric(data$NetDiv)
data$ADV<-as.numeric(data$ADV)
data$RD<-as.numeric(data$RD)
data$BM<-as.numeric(data$BM)
data <- na.omit(data)

# 假設你的資料框架名為 data
ps_model <- glm(RPA ~ Earnings + NetDiv + ADV + RD + ROA + S + SG + BM + Year + Industry, data = data)

# 提取傾向分數
data$propensity_score <- predict(ps_model, type = "response")

# 執行傾向分數匹配
matched_data <- matchit(RPA ~ Earnings + NetDiv + ADV + RD + ROA + S + SG + BM + Year + Industry  , data = data, method = "nearest")

# 從匹配物件中提取匹配後的資料框架
data <- match.data(matched_data)

# Winsorize each continuous variable

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

data$MVE<-winsorize(data$MVE)
data$BVE<-winsorize(data$BVE)
data$Earnings<-winsorize(data$Earnings)
data$NetDiv<-winsorize(data$NetDiv)
data$ADV<-winsorize(data$ADV)
data$RPA_Count<-winsorize(data$RPA_Count)
data$RD<-winsorize(data$RD)
data$ROA<-winsorize(data$ROA)
data$S<-winsorize(data$S)
data$SG<-winsorize(data$SG)
data$BM<-winsorize(data$BM)
data$RPA <- factor(data$RPA)


# Now, perform the Huber regression or any regression analysis using winsorized variables
model <- lm(log(MVE) ~ RPA + Earnings + NetDiv + ADV + RD + ROA + S + SG + BM + Year + Industry, data = data)
summary(model)

#
# 將結果儲存到文字檔案cat(summary_text, sep = "\n")
summary_text <- capture.output(summary(model))
sink("Muhanna_summary_PSM_FixHetero.txt")
summary(model)
coeftest(model, vcov = vcovHC(model, type="HC3"))
sink()


#
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
coeftest(model, vcov = vcovHC(model, type="HC1"))

#No Autocorrelation of Residuals
bgtest(model)

# 將結果儲存到文字檔案
summary_text <- capture.output(summary(model))
sink("Muhanna_summary_0112_PSM.txt")
cat(summary_text, sep = "\n")
coeftest(model, vcov = vcovHC(model, type="HC3"))
sink()
