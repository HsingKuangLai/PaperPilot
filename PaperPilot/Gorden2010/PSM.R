# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("data.csv", na.strings = "#N/A")

# 移除含有缺失值的觀測值
data <- na.omit(data)
data$Year <- factor(data$Year)
data$Industry <- factor(data$Industry)
data$Finance <- factor(data$Finance)
data$dEPSS<-as.numeric(data$dEPSS)
data$EPSS<-as.numeric(data$EPSS)
data$Annret<-as.numeric(data$Annret)
data <- na.omit(data)

# 假設你的資料框架名為 data
ps_model <- glm(RPA ~ ( EPSS + dEPSS + dNEG + Year + Industry) , data = data, family=binomial("logit"))

# 提取傾向分數
data$propensity_score <- predict(ps_model, type = "response")

# 執行傾向分數匹配
matched_data <- matchit(RPA ~ ( EPSS  + dEPSS + dNEG + Year + Industry)   , data = data, method = "nearest")

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

data_PSM$Annret<-winsorize(data_PSM$Annret)
data_PSM$EPSS<-winsorize(data_PSM$EPSS)
data_PSM$dEPSS<-winsorize(data_PSM$dEPSS)
data_PSM$RPA <- factor(data_PSM$RPA)

# Now, perform the Huber regression or any regression analysis using winsorized variables
model <- lm(Annret ~ RPA + ( EPSS  + dEPSS + dNEG + Year + Industry) , data = data_PSM)
summary(model)
