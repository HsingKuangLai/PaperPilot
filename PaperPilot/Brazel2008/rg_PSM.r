library(sandwich)
library(lmtest)
library(MASS)
library(Matching)
library(rgenoud)
library(MatchIt)
# read CSV
data <- read.csv("data.csv", na.strings = "#N/A")

# data preprocessing
data <- na.omit(data)
data$RPA_Ctd <- factor(data$RPA_Ctd)
data$RPA <- factor(data$RPA)
data$GC <- factor(data$GC)
data$Big4 <- factor(data$Big4)
data$Industry <- factor(data$Industry)
data$Year <- factor(data$Year)
data$Finance <- factor(data$Finance)
data$ABSDA<-as.numeric(data$ABSDA)
data$ADV<-as.numeric(data$ADV)
data$ABSDA_ROA<-abs(as.numeric(data$DA_ROA))
data$RAM<-data$ABCFO+data$ABEXP
data$OCF<-as.numeric(data$OCF)
data$Age<-log(1+(data$Age))
data$Age_Trade<-log(1+(data$Age_Trade))
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


# Identify columns that are numeric and not factor
numeric_cols <- sapply(data, is.numeric) & !sapply(data, is.factor)
# Apply the winsorize function to the selected numeric columns
data[numeric_cols] <- lapply(data[numeric_cols], winsorize)
data$ADJROA_sq<-data$ADJROA*data$ADJROA
data$RPA<-as.numeric(data$RPA)-1
data$Year<-as.numeric(data$Year)-1


###second step mataching

library(MatchIt)
## generate psm score by glm 
ps_model <- glm(RPA ~ (ABSDA_ROA + LEV + OCF + MTB  + ADJROA + LGTA + Age + RD + ADV + ESG + Big4 + GC ) + Year, data = data)
data$propensity_score <- predict(ps_model)

# matching parameter method=nn, distance=glm, link=probit, caliper=NULL
matched_data<- matchit( RPA ~ (ABSDA_ROA + LEV + OCF + MTB  + ADJROA + LGTA + Age + RD + ADV + ESG + Big4 + GC  ) + Year , data = data, method = "nearest",distance = "glm")
data <- match.data(matched_data)

###
#sink("PSM_AM.txt")
summary(ps_model)
model <- (lm((RAM) ~ RPA  + (ABSDA_ROA + LEV + OCF + MTB  + ADJROA  + LGTA + Age + RD + ADV + ESG + Big4) + Year   , data = data))
summary(model)
coeftest(model, vcov = vcovHC(model))
coeftest(model, vcov = vcovCL(model,cluster = ~Key))
#sink()





