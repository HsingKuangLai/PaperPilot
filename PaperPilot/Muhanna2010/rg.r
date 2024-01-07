# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("data.csv", na.strings = "#N/A")

# 移除含有缺失值的觀測值
data <- na.omit(data)
data$RPA <- factor(data$RPA)
data$Year <- factor(data$Year)
data$Industry <- factor(data$Industry)
data$Finance <- factor(data$Finance)
data$NetDiv<-as.numeric(data$NetDiv)
data$ADV<-as.numeric(data$ADV)
data$RD<-as.numeric(data$RD)
data$BM<-as.numeric(data$BM)
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

data$MVE<-winsorize(data$MVE)
data$BVE<-winsorize(data$BVE)
data$Earnings<-winsorize(data$Earnings)
data$NetDiv<-winsorize(data$NetDiv)
data$ADV<-winsorize(data$ADV)
data$RD<-winsorize(data$RD)
data$ROA<-winsorize(data$ROA)
data$S<-winsorize(data$S)
data$SG<-winsorize(data$SG)
data$BM<-winsorize(data$BM)


# Now, perform the Huber regression or any regression analysis using winsorized variables
model <- lm(MVE ~ RPA + BVE + Earnings + NetDiv + ADV + RD + ROA + S + SG + BM + Year + Industry , data = data)
summary(model)


# 將結果儲存到文字檔案
summary_text <- capture.output(summary(model))
sink("regression_summary.txt")
cat(summary_text, sep = "\n")
sink()