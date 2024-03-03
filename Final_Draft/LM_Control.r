library(sandwich)
library(lmtest)
library(MASS)
library(dplyr)
library(stargazer)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("Ctd_Control.csv")

# 移除含有缺失值的觀測值
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABSDA2<-abs(data$DA2)
data$ABCFO<-data$ABCFO*(-1)
data$ABEXP<-data$ABEXP*(-1)
data$RM<-data$ABCFO+data$ABEXP+data$ABPROD
data$RM1<-data$ABCFO+data$ABEXP
data$RM2<-data$ABEXP+data$ABPROD
data$MVE<-data$Equity/data$Asset_1
data$ABSRM<-abs(data$RM)
data$ESG<-log(data$ESG)
data$Suspect <- ifelse(data$ROA < 0.005 & data$ROA > 0, 1, 0)
data$Loss <- ifelse(data$ROA < 0 , 1, 0)
########### winsorizing 1% 
# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}


# Factorize the first 6 columns
data[1:10] <- lapply(data[1:10], factor)

# Apply the winsorize function to the remaining columns
data <- data %>%
  mutate(across(.cols = 11:ncol(data), .fns = ~winsorize(.)))

data$ADJROA_sq<-data$ADJROA*data$ADJROA
data$ROA_sq<-data$ROA*data$ROA
data$Age_sq<-data$Age*data$Age
data$Year<-(as.numeric(data$Year)+2016)
#sink("Result_EBITDA_1.txt")

sink("Result_NICtd_withControlGroup_ABPROD.txt")



# Proxy names for AM and RM
AM_proxy <- "ABSDA"  # Substitute 'ABSDA1' with any other AM proxy as needed
RM_proxy <- "ABPROD"      # Substitute 'RM' with any other RM proxy as needed

print(RM_proxy)

# Define control variables , "ESG",, "Zscore"
control_vars <- c("RPA_Ctd","Suspect", "NOA", "INST", "MS", "LEV", "OCF", "MTB", "SG", "ADJROA", "ADJROA_sq", "ADV","LGTA", "Big4")
control_vars_AM <- c("RPA_Ctd","Adopt","Adopt_RPA","SEO","NOA","INST","Cycle","Zscore","CL","MS","OCF","MTB","LEV","ADJROA", "ADJROA_sq", "LGTA", "Big4","Year")
control_vars_RM <- c("RPA_Ctd","Adopt","Adopt_RPA","SEO","NOA", "INST","Cycle","Zscore", "CL","MS","OCF","LEV", "MTB", "ADJROA", "ADJROA_sq", "ADV","RD", "LGTA","Year")


# Model for AM with control variables and AM proxy
modelAM_HAT_formula <- as.formula(paste(AM_proxy, "~ ", paste(control_vars_AM, collapse=" + ")))
modelAM_HAT <- lm(modelAM_HAT_formula, data = data,singular.ok = FALSE)
data$AMhat <- fitted.values(modelAM_HAT)
data$AMres <- residuals(modelAM_HAT)
# Model for RM without AM.hat to get RM.hat
modelRM_HAT_formula <- as.formula(paste(RM_proxy, "~ ", paste(control_vars_RM, collapse=" + ")))
modelRM_HAT <- lm(modelRM_HAT_formula, data = data)
data$RMhat <- fitted(modelRM_HAT)
data$RMres <- residuals(modelRM_HAT)
# Include Year in control variables for RM models
control_vars_with_year <- c(control_vars, "Year")

# Model for RM with AM.hat and control variables
modelRM_formula <- as.formula(paste(RM_proxy, "~ AMhat +", paste(control_vars_RM, collapse=" + ")))
modelRM <- lm(modelRM_formula, data = data)
print("Endogenity Test:")
summary(lm(paste(RM_proxy,"~ABSDA+AMres"),data=data))
print("1st Stage:")
summary(modelRM_HAT)
coeftest(modelRM_HAT, vcov = vcovHC(modelRM_HAT, type = "HC0"))
print("2nd Stage:")
summary(modelRM)
coeftest(modelRM, vcov = vcovHC(modelRM, type = "HC0"))


# Model for AM with RM.hat, control variables, and AM proxy
modelAM_formula <- as.formula(paste(AM_proxy, "~   RMhat + ", paste(control_vars_AM, collapse=" + ")))
modelAM <- lm(modelAM_formula, data = data)
print("Endogenity Test:")
summary(lm(paste("ABSDA~",RM_proxy,"+RMres"),data=data))
print("1st Stage:")
summary(modelAM_HAT)
coeftest(modelAM_HAT, vcov = vcovHC(modelAM_HAT, type = "HC0"))
print("2nd Stage:")
summary(modelAM)
coeftest(modelAM, vcov = vcovHC(modelAM, type = "HC0"))

sink()