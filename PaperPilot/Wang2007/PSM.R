# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("data.csv", na.strings = "#N/A")

# 移除含有缺失值的觀測值
data <- na.omit(data)
data$Year <- factor(data$Year)
data$Industry <- factor(data$Industry)
data$Finance <- factor(data$Finance)
data$BV<-as.numeric(data$BV)
data$E<-as.numeric(data$E)
data$CAP<-as.numeric(data$CAP)
data$RD<-as.numeric(data$RD)
data$BM<-as.numeric(data$BM)
data$TA<-as.numeric(data$TA)
data <- na.omit(data)


# 假設你的資料框架名為 data
ps_model <- glm(RPA ~ BV + E + CAP + RD + delSales + ROA + BM + TA + Year + Industry , data = data)

# 提取傾向分數
data$propensity_score <- predict(ps_model, type = "response")

# 執行傾向分數匹配
matched_data <- matchit(RPA ~BV + E + CAP + RD + delSales + ROA + BM + TA + Year + Industry, data = data, method = "nearest")

# 從匹配物件中提取匹配後的資料框架
data_PSM <- match.data(matched_data)

# Winsorize each continuous variable

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

data_PSM$RPA<-factor(data_PSM$RPA)
data_PSM$Price<-winsorize(data_PSM$Price)
data_PSM$BV<-winsorize(data_PSM$BV)
data_PSM$E<-winsorize(data_PSM$E)
data_PSM$CAP<-winsorize(data_PSM$CAP)
data_PSM$RD<-winsorize(data_PSM$RD)
data_PSM$delSales<-winsorize(data_PSM$delSales)
data_PSM$ROA<-winsorize(data_PSM$ROA)
data_PSM$BM<-winsorize(data_PSM$BM)
data_PSM$TA<-winsorize(data_PSM$TA)

# Now, perform the Huber regression or any regression analysis using winsorized variables
model <- lm(Price ~ RPA + BV + E + CAP + RD + delSales + ROA + BM + TA + Year + Industry, data = data)
summary(model)
