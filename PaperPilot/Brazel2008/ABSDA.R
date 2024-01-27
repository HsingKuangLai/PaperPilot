# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("ABEXP.csv")

data$EXP<-as.numeric(data$EXP)
data$A1<-as.numeric(data$A1)
data$A2<-as.numeric(data$A2)
#data$A3<-as.numeric(data$A3)
#data$A4<-as.numeric(data$A4)
data<-na.omit(data)

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

data$EXP<-winsorize(data$EXP)
data$A1<-winsorize(data$A1)
data$A2<-winsorize(data$A2)
#data$A3<-winsorize(data$A3)
#data$A4<-winsorize(data$A4)


model<-lm(EXP~ A1+A2,data=data)
summary(model)

data$ABEXP<-residuals(model)

write.csv(data,"ABEXP.csv")