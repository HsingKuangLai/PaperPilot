library(sandwich)
library(lmtest)
library(MASS)
library(Matching)
library(rgenoud)
library(MatchIt)
# read CSV
data <- read.csv("data - 複製.csv", na.strings = "#N/A")

# data preprocessing
data <- na.omit(data)
data$RPA_Ctd <- factor(data$RPA_Ctd)
#data$RPA <- factor(data$RPA)
data$GC <- factor(data$GC)
data$Big4 <- factor(data$Big4)
data$Industry <- factor(data$Industry)
#data$Year <- factor(data$Year)
data$Finance <- factor(data$Finance)
data$ABSDA<-as.numeric(data$ABSDA)
data$ADV<-as.numeric(data$ADV)
data$ABSDA_ROA<-abs(as.numeric(data$DA_ROA))
data$RAM<-data$ABCFO+data$ABEXP
data$OCF<-as.numeric(data$OCF)
data$ADJROA_sq<-data$ADJROA^2
data <- na.omit(data)

######fisrt step######################### winsorizing

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}


#Winsorize
data$DA<-winsorize(data$DA)
data$DA_ROA<-winsorize(data$DA_ROA)
data$ABSDA_ROA<-winsorize(data$ABSDA_ROA)
data$ABSDA<-winsorize(data$ABSDA)
data$ABCFO<-winsorize(data$ABCFO)
data$ABPROD<-winsorize(data$ABPROD)
data$ABEXP<-winsorize(data$ABEXP)
data$RAM<-winsorize(data$RAM)
data$LGTA<-winsorize(data$LGTA)
data$LEV<-winsorize(data$LEV)
data$OCF<-winsorize(data$OCF)
data$MTB<-winsorize(data$MTB)
data$ESG<-winsorize(data$ESG)
data$Age<-winsorize(data$Age)
data$ROA<-winsorize(data$ROA)
data$RD<-winsorize(data$RD)
data$ADV<-winsorize(data$ADV)
data$ADJROA<-winsorize(data$ADJROA)
data$ADJROA_sq<-data$ADJROA^2
data$Age<-log(1+winsorize(data$Age))
data$Age_Trade<-log(1+winsorize(data$Age_Trade))
data$RPA_Count<-winsorize(data$RPA_Count)


###second step mataching

library(MatchIt)
## generate psm score by glm 
ps_model <- glm(RPA ~ (RAM + LEV + OCF + MTB  + ADJROA + LGTA + Age + RD + ADV + ESG + Big4 + GC ) + Year, data = data)
data$propensity_score <- predict(ps_model)

# matching parameter method=nn, distance=glm, link=probit, caliper=NULL
matched_data<- matchit( RPA ~ (RAM + LEV + OCF + MTB  + ADJROA + LGTA + Age + RD + ADV + ESG + Big4 + GC  ) + Year , data = data, method = "nearest",distance = "glm")
data <- match.data(matched_data)

###
sink("PSM_AM.txt")
summary(ps_model)
model <- (lm((ABSDA_ROA) ~ RPA  + (RAM + LEV + OCF + MTB  + ADJROA  + LGTA + Age + RD + ADV + ESG + Big4) + Year   , data = data))
summary(model)
coeftest(model, vcov = vcovHC(model))
coeftest(model, vcov = vcovCL(model,cluster = ~Key))
sink()





