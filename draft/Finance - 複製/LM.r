library(sandwich)
library(lmtest)
library(MASS)
library(dplyr)
library(stargazer)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("Total.csv")

# 移除含有缺失值的觀測值
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABSDA2<-abs(data$DA2)
data$ABCFO<-data$ABCFO*(-1)
data$ABEXP<-data$ABEXP*(-1)
data$RM<-data$ABCFO+data$ABEXP+data$PROD
data$RM1<-data$ABCFO+data$ABEXP
data$RM2<-data$ABPROD+data$ABEXP

data$ABSRM<-abs(data$RM)

########### winsorizing 1% 
# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.05)  # 1st percentile value
  p99 <- quantile(x, probs = 0.95)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

data<-subset(data,data$Adopt==1 & data$Fin==0)

# Factorize the first 6 columns
data[1:9] <- lapply(data[1:9], factor)

# Apply the winsorize function to the remaining columns
data <- data %>%
  mutate(across(.cols = 10:ncol(data), .fns = ~winsorize(.)))

data$ADJROA_sq<-data$ADJROA*data$ADJROA
data$ROA_sq<-data$ROA*data$ROA
data$Age_sq<-data$Age*data$Age

data$Year<-as.numeric(data$Year)

#sink("Result_EBITDA_PROXYWIN.txt")

modelAM<-(lm(formula = ABSDA1 ~ RPA_Ctd * RM   + Zscore + CL + Cycle + 
                 LEV + OCF + MTB + ROA + ROA_sq + LGTA + ADV  + Age + Big4 + Year, data = data))

coeftest(modelAM, vcov = vcovHC(modelAM, type = "HC0"))

modelRM<-(lm(formula = RM ~ RPA_Ctd * ABSDA1  + Zscore  + CL + Cycle + 
                 LEV + OCF + MTB + ROA + ROA_sq + LGTA  + ADV + Age + Big4 + Year, data = data))
coeftest(modelRM, vcov = vcovHC(modelRM, type = "HC0"))
#sink()