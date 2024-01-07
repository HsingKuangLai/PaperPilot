# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("data_6m.csv", na.strings = "#N/A")

# 移除含有缺失值的觀測值
data <- na.omit(data)

data$RPA <- factor(data$RPA)
data$Year <- factor(data$Year)
data$Industry <- factor(data$Industry)
data$BVE<-as.numeric(data$BVE)
data$NIS<-as.numeric(data$NIS)
data$LNPrice<-log(data$Price)
data$KY <- factor(data$KY)
data$RPA_Count_sq<-data$RPA_Count*data$RPA_Count
#data$Finance <- factor(data$Finance)
#^M28|^M30000|
data <- subset(data, !(grepl("^M28|^M3000", Industry)))


# Now, perform the Huber regression or any regression analysis using winsorized variables
model <- lm(Price ~ BVE + LNAT + NIS + RPA + Year + Industry , data = data)
summary(model)



# 進行迴歸分析
model <- lm(Price ~ BVE + LNAT + NIS + RPA + KY + Year + Industry, data = data)

summary(model)

# 將結果儲存到文字檔案
summary_text <- capture.output(summary(model))
sink("regression_summary.txt")
cat(summary_text, sep = "\n")
sink()

############################### winsorizing 1% greater (But equal to dummy)

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

# Winsorize each continuous variable
data$BVE <- winsorize(data$BVE)
data$LNAT <- winsorize(data$LNAT)
data$NIS <- winsorize(data$NIS)
data$LN_RPA_Count <- winsorize(data$LN_RPA_Count)
data$RPA_Count <- winsorize(data$RPA_Count)
data$RPA_Count_sq<-data$RPA_Count*data$RPA_Count


# Now, perform the Huber regression or any regression analysis using winsorized variables
model <- lm(Price ~ BVE + LNAT + NIS + RPA_Count + Year + Industry, data = data)
summary(model)

# Now, perform the Huber regression or any regression analysis using winsorized variables
model <- lm(Price ~ BVE + LNAT + NIS + RPA_Count + RPA_Count_sq + Year, data = data)
summary(model)


