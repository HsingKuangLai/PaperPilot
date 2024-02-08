library(dplyr)
library(stargazer)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("clean.csv")


# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

#Jones/MJONES/MJONES_ROA/MJONES_BMCFO
TAC<-data[,c(1,2,3,4,5,6)]
TAC$AC<-(data$NI_Ctd-data$OCF)/data$Asset_1
TAC$A1<-1/data$Asset_1
TAC$A21<-(data$S-data$S_1)/data$Asset_1
TAC$A2<-(data$S-data$S_1-(data$AR-data$AR_1))/data$Asset_1
TAC$A3<-data$PPE/data$Asset_1
TAC$A4<-data$NI_Ctd/data$Asset_1
TAC$A5<-data$BM
TAC$A6<-data$OCF/data$Asset_1
TAC$ROA<-data$NI_Ctd/data$Asset_1

# Assuming your dataframe is named 'data' and it has columns 'ROA', 'Industry', and 'Year'
TAC <- TAC %>%
  group_by(Industry, Year) %>%
  mutate(
    Median_ROA = median(ROA, na.rm = TRUE), # Calculate median of ROA for each Industry-Year group
    ADJROA = ROA - Median_ROA # Subtract median ROA from ROA for each row
  ) %>%
  ungroup() # Ungroup the data frame

model<-lm(AC~ A1+A21+A3,data=TAC)
model1<-lm(AC~ A1+A2+A3,data=TAC)
model2<-lm(AC~ A1+A2+A3+A4,data=TAC)
model3<-lm(AC~ A1+A2+A3+A5+A6,data=TAC)
summary(model)
summary(model1)
summary(model2)
summary(model3)
TAC$DA<-residuals(model)
TAC$DA1<-residuals(model1)
TAC$DA2<-residuals(model2)
TAC$DA3<-residuals(model3)

# Output all models in a single table
stargazer(model2, type = "html", 
          title = "DA", out = "AMproxy.html")


models=list()

RM<-data[,c(1,2,3)]
RM$CFO<-data$OCF/data$Asset_1
RM$PROD<-data$PROD/data$Asset_1
RM$EXP<-data$EXP/data$Asset_1
RM$A1<-1/data$Asset_1
RM$A2<-data$S/data$Asset_1
RM$A3<-(data$S-data$S_1)/data$Asset_1
RM$A4<-(data$S_1-data$S_2)/data$Asset_1
RM$A5<-data$S_1/data$Asset_1

model1<-lm(CFO~ A1+A2+A3,data=RM)
model2<-lm(PROD~ A1+A2+A3+A4,data=RM)
model3<-lm(EXP~ A1+A5,data=RM)
summary(model1)
summary(model2)
summary(model3)
RM$ABCFO<-residuals(model1)
RM$ABPROD<-residuals(model2)
RM$ABEXP<-residuals(model3)


# Output all models in a single table
stargazer(model1,model2,model3, type = "html", 
          title = "RM", out = "RMproxy.html")

write.csv(TAC,"TAC_NoWinsorize.csv")
write.csv(RM,"RM_Nowinsorize.csv")

TAC<-TAC[,c("Key","DA","DA1","DA2","DA3","ROA","Median_ROA","ADJROA")]
RM<-RM[,c("Key","ABCFO","ABPROD","ABEXP")]
TACRM<-merge(TAC,RM,by="Key")

rawdata <- read.csv("data.csv")
Total<-merge(rawdata,TACRM,by="Key")

write.csv(Total,"Total.csv")

