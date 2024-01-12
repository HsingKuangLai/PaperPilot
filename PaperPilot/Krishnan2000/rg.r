# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("data.csv", na.strings = "#N/A")

# 移除含有缺失值的觀測值
data <- na.omit(data)

data$RPA <- factor(data$RPA)
data$Year <- factor(data$Year)
data$Industry <- factor(data$Industry)
data$Finance <- factor(data$Finance)
data$size<-log(data$TA)/data$Equity
data <- subset(data, !(grepl("^M28|^M3000", Industry)))

############################### winsorizing 1% greater (But equal to dummy)

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

data$MBV<-winsorize(data$MBV)
data$EARNBV<-winsorize(data$EARNBV)
data$size<-winsorize(data$size)
data$RPA_Count_sq<-data$RPA_Count*data$RPA_Count

# Now, perform the Huber regression or any regression analysis using winsorized variables
model <- lm(MBV ~ EARNBV + Year + RPA + Industry + size, data = data)
summary(model)


# 將結果儲存到文字檔案
summary_text <- capture.output(summary(model))
sink("Krishan_Size.txt")
cat(summary_text, sep = "\n")
sink()