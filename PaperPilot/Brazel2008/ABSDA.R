# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("ABSDA.csv")

data$TAC<-as.numeric(data$TAC)
data$A1<-as.numeric(data$A1)
data$A2<-as.numeric(data$A2)
data$A3<-as.numeric(data$A3)
data<-na.omit(data)

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

data$TAC<-winsorize(data$TAC)
data$A1<-winsorize(data$A1)
data$A2<-winsorize(data$A2)
data$A3<-winsorize(data$A3)


model<-lm(TAC~ A1+A2+A3,data=data)
summary(model)

data$DA<-residuals(model)
data$ABSDA<-sqrt(data$DA*data$DA)