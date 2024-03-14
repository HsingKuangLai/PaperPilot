library(sandwich)
library(lmtest)
library(MASS)
library(dplyr)
library(stargazer)
library(multcomp)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("Ctd_Control.csv")

# 移除含有缺失值的觀測值
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABSDA2<-abs(data$DA2)
data$ABEXP<-data$ABEXP*(-1)
data$RM<-data$ABEXP+data$ABPROD
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
data$YEAR<-(as.numeric(data$YEAR)+2016)


# Proxy names for AM and RM
AM_proxy <- "ABSDA"  # Substitute 'ABSDA1' with any other AM proxy as needed
RM_proxies <- c("ABPROD","ABEXP","RM")      # Substitute 'RM' with any other RM proxy as needed

model_endo<-list()
model_fst<-list()
model_snd<-list()
rst_endo<-list()
rst_fst<-list()
rst_snd<-list()

for (RM_proxy in RM_proxies) {
  
  # Define control variables , "ESG",, "Zscore"
  control_vars <- c("RPA_Ctd","Suspect", "NOA", "INST", "MS", "LEV", "OCF", "MTB", "SG", "ADJROA", "ADJROA_sq", "ADV","LGTA", "Big4")
  control_vars_AM <- c("POST","RPA","POST_RPA","NOA","INST","CYCLE","ZSCORE","CL","MS","OCF","MTB","LEV","ADJROA", "ADJROA_sq", "LGTA", "BIG4","YEAR")
  control_vars_RM <- c("POST","RPA","POST_RPA","NOA", "INST","CYCLE","ZSCORE", "CL","MS","OCF","LEV", "MTB", "ADJROA", "ADJROA_sq", "ADV","RD", "LGTA","YEAR")
  
  # Model for AM with control variables and AM proxy
  modelAM_HAT_formula <- as.formula(paste(AM_proxy, "~ ", paste(control_vars_AM, collapse=" + ")))
  modelAM_HAT <- lm(modelAM_HAT_formula, data = data,singular.ok = FALSE)
  
  data$AMhat <- fitted.values(modelAM_HAT)
  data$AMres <- residuals(modelAM_HAT)
  # Model for RM without AM.hat to get RM.hat
  modelRM_HAT_formula <- as.formula(paste(RM_proxy, "~", paste(control_vars_RM, collapse=" + ")))
  modelRM_HAT <- lm(modelRM_HAT_formula, data = data)
  data$RMhat <- fitted(modelRM_HAT)
  data$RMres <- residuals(modelRM_HAT)
  # Include Year in control variables for RM models
  control_vars_with_year <- c(control_vars, "Year")
  
  # Model for RM with AM.hat and control variables
  modelRM_formula <- as.formula(paste(RM_proxy, "~ AMhat +", paste(control_vars_RM, collapse=" + ")))
  modelRM <- lm(modelRM_formula, data = data)
  modelRM_endo<-lm(paste(RM_proxy,"~ABSDA+AMres"),data=data)
  
  #RM_endo
  model_endo[[paste("RM_",RM_proxy)]]<-modelRM_endo
  cov<-vcovHC(modelRM_endo,type="HC0")
  rst_endo[[paste("RM_",RM_proxy)]]<-sqrt(diag(cov))
  
  #1st
  model_fst[[paste("RM_",RM_proxy)]]<-modelRM_HAT
  cov<-vcovHC(modelRM_HAT,type="HC0")
  rst_fst[[paste("RM_",RM_proxy)]]<-sqrt(diag(cov))
  
  #2nd
  model_snd[[paste("RM_",RM_proxy)]]<-modelRM
  cov<-vcovHC(modelRM,type="HC0")
  rst_snd[[paste("RM_",RM_proxy)]]<-sqrt(diag(cov))
  
  
  # Model for AM with RM.hat, control variables, and AM proxy
  modelAM_formula <- as.formula(paste(AM_proxy, "~   RMhat + ", paste(control_vars_AM, collapse=" + ")))
  modelAM <- lm(modelAM_formula, data = data)
  modelAM_endo<-lm(paste("ABSDA ~ RMres + ", RM_proxy),data=data)
  
  #AM_endo
  model_endo[[paste("AM_",RM_proxy)]]<-modelAM_endo
  cov<-vcovHC(modelAM_endo,type="HC0")
  rst_endo[[paste("AM_",RM_proxy)]]<-sqrt(diag(cov))
  
  #1st
  model_fst[[paste("AM_",RM_proxy)]]<-modelAM_HAT
  cov<-vcovHC(modelAM_HAT,type="HC0")
  rst_fst[[paste("AM_",RM_proxy)]]<-sqrt(diag(cov))
  
  #2nd
  model_snd[[paste("AM_",RM_proxy)]]<-modelAM
  cov<-vcovHC(modelAM,type="HC0")
  rst_snd[[paste("AM_",RM_proxy)]]<-sqrt(diag(cov))
  # Specify the linear hypothesis
  glht_mod_AM <- glht(model = modelAM, linfct = c("POST1 + RPA1 + POST_RPA1 = 0"))
  #print(summary(glht_mod_AM))
  glht_mod_RM <- glht(model = modelRM, linfct = c("POST1 + RPA1 +POST_RPA1 = 0"))
  #print(summary(glht_mod_RM))
  
}

# Output all models in a single table
stargazer(rev(model_endo)[-c(5,3)], type = "html", report=('vc*stp'),column.labels = NULL,
          se = rev(rst_endo)[-c(5,3)], 
          title = "Endogeneity Test", out = "Endo_Ctrl.html")

# Output all models in a single table
stargazer(rev(model_fst)[-c(5,3)], type = "html",report=('vc*stp'), column.labels = NULL,
          se = rev(rst_fst)[-c(5,3)], 
          title = "First Stage", out = "fst_Ctrl.html")

# Output all models in a single table
stargazer(rev(model_snd)[-c(5,3)], type = "html",report=('vc*stp'), column.labels = NULL,
          se = rev(rst_snd)[-c(5,3)], 
          title = "Second Stage", out = "snd_Ctrl.html")


data<-subset(data,data$RPA==1)

# Proxy names for AM and RM
AM_proxy <- "ABSDA"  # Substitute 'ABSDA1' with any other AM proxy as needed
RM_proxies <- c("ABPROD","ABEXP","RM")      # Substitute 'RM' with any other RM proxy as needed

model_endo<-list()
model_fst<-list()
model_snd<-list()
rst_endo<-list()
rst_fst<-list()
rst_snd<-list()

for (RM_proxy in RM_proxies) {
  
  # Define control variables , "ESG",, "Zscore"
  control_vars <- c("RPA_Ctd","Suspect", "NOA", "INST", "MS", "LEV", "OCF", "MTB", "SG", "ADJROA", "ADJROA_sq", "ADV","LGTA", "Big4")
  control_vars_AM <- c("POST","NOA","INST","CYCLE","ZSCORE","CL","MS","OCF","MTB","LEV","ADJROA", "ADJROA_sq", "LGTA", "BIG4","YEAR")
  control_vars_RM <- c("POST","NOA", "INST","CYCLE","ZSCORE", "CL","MS","OCF","LEV", "MTB", "ADJROA", "ADJROA_sq", "ADV","RD", "LGTA","YEAR")
  
  # Model for AM with control variables and AM proxy
  modelAM_HAT_formula <- as.formula(paste(AM_proxy, "~ ", paste(control_vars_AM, collapse=" + ")))
  modelAM_HAT <- lm(modelAM_HAT_formula, data = data,singular.ok = FALSE)
  
  data$AMhat <- fitted.values(modelAM_HAT)
  data$AMres <- residuals(modelAM_HAT)
  # Model for RM without AM.hat to get RM.hat
  modelRM_HAT_formula <- as.formula(paste(RM_proxy, "~", paste(control_vars_RM, collapse=" + ")))
  modelRM_HAT <- lm(modelRM_HAT_formula, data = data)
  data$RMhat <- fitted(modelRM_HAT)
  data$RMres <- residuals(modelRM_HAT)
  # Include Year in control variables for RM models
  control_vars_with_year <- c(control_vars, "Year")
  
  # Model for RM with AM.hat and control variables
  modelRM_formula <- as.formula(paste(RM_proxy, "~ AMhat +", paste(control_vars_RM, collapse=" + ")))
  modelRM <- lm(modelRM_formula, data = data)
  modelRM_endo<-lm(paste(RM_proxy,"~ABSDA+AMres"),data=data)
  
  #RM_endo
  model_endo[[paste("RM_",RM_proxy)]]<-modelRM_endo
  cov<-vcovHC(modelRM_endo,type="HC0")
  rst_endo[[paste("RM_",RM_proxy)]]<-sqrt(diag(cov))
  
  #1st
  model_fst[[paste("RM_",RM_proxy)]]<-modelRM_HAT
  cov<-vcovHC(modelRM_HAT,type="HC0")
  rst_fst[[paste("RM_",RM_proxy)]]<-sqrt(diag(cov))
  
  #2nd
  model_snd[[paste("RM_",RM_proxy)]]<-modelRM
  cov<-vcovHC(modelRM,type="HC0")
  rst_snd[[paste("RM_",RM_proxy)]]<-sqrt(diag(cov))
  
  
  # Model for AM with RM.hat, control variables, and AM proxy
  modelAM_formula <- as.formula(paste(AM_proxy, "~   RMhat + ", paste(control_vars_AM, collapse=" + ")))
  modelAM <- lm(modelAM_formula, data = data)
  modelAM_endo<-lm(paste("ABSDA ~ RMres + ", RM_proxy),data=data)
  
  #AM_endo
  model_endo[[paste("AM_",RM_proxy)]]<-modelAM_endo
  cov<-vcovHC(modelAM_endo,type="HC0")
  rst_endo[[paste("AM_",RM_proxy)]]<-sqrt(diag(cov))
  
  #1st
  model_fst[[paste("AM_",RM_proxy)]]<-modelAM_HAT
  cov<-vcovHC(modelAM_HAT,type="HC0")
  rst_fst[[paste("AM_",RM_proxy)]]<-sqrt(diag(cov))
  
  #2nd
  model_snd[[paste("AM_",RM_proxy)]]<-modelAM
  cov<-vcovHC(modelAM,type="HC0")
  rst_snd[[paste("AM_",RM_proxy)]]<-sqrt(diag(cov))
}

# Output all models in a single table
stargazer(rev(model_endo)[-c(5,3)], type = "html",report=('vc*stp'), column.labels = NULL,
          se = rev(rst_endo)[-c(5,3)], 
          title = "Endogeneity Test", out = "Endo.html")

# Output all models in a single table
stargazer(rev(model_fst)[-c(5,3)], type = "html",report=('vc*stp'), column.labels = NULL,
          se = rev(rst_fst)[-c(5,3)], 
          title = "First Stage", out = "fst.html")

# Output all models in a single table
stargazer(rev(model_snd)[-c(5,3)], type = "html",report=('vc*stp'), column.labels = NULL,
          se = rev(rst_snd)[-c(5,3)], 
          title = "Second Stage", out = "snd.html")