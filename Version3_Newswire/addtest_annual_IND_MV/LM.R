library(sandwich)
library(lmtest)
library(MASS)
library(dplyr)
library(stargazer)
library(multcomp)
library(car)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("base.csv")
AQdata<-read.csv("SecondEq.csv")
AQdata<-AQdata[,c("KEY","AQ","IAQ","DAQ")]
data<-inner_join(data,AQdata,by="KEY")

# 移除含有缺失值的觀測值
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABEXP<-data$ABEXP*(-1)
data$ABCFO<-data$ABCFO*(-1)
data$RM<-(data$ABEXP+data$ABPROD)
data$SIZE<-log(data$MV)


########### winsorize 1% 
# Define a function for winsorize
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}


# Factorize the first 6 columns
data[1:12] <- lapply(data[1:12], factor)

# Apply the winsorize function to the remaining columns
data <- data %>%
  mutate(across(.cols = 13:ncol(data), .fns = ~winsorize(.)))

data$ADJROA_sq<-data$ADJROA*data$ADJROA
data$YEAR<-(as.numeric(data$YEAR)+2016)

# Proxy names for AM and RM
AM_proxy <- "DAQ"  # Substitute 'ABSDA1' with any other AM proxy as needed
RM_proxies <- c("ABPROD","ABEXP","RM")      # Substitute 'RM' with any other RM proxy as needed

model_endo<-list()
model_fst<-list()
model_snd<-list()
rst_endo<-list()
rst_fst<-list()
rst_snd<-list()

for (RM_proxy in RM_proxies) {
  
  # Define control variables , "ESG",, "Zscore"
  control_vars <- c("RPA_Ctd", "NOA", "INST", "MS", "LEV", "OCF", "MTB", "SG", "ADJROA", "ADJROA_sq", "ADV","SIZE", "Big4")
  control_vars_AM <- c("POST","RPA","POST_RPA","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL", "ADJROA", "ADJROA_sq","SIZE","BIG4","YEAR")
  control_vars_RM <- c("POST","RPA","POST_RPA","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL", "MTB", "ADJROA", "ADJROA_sq", "SIZE", "ADV","RD","YEAR")
  
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
  modelRM_endo<-lm(paste(RM_proxy,"~",AM_proxy,"+", paste(control_vars_RM, collapse=" + ")),data=data)
  
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
  modelAM_endo<-lm(paste(AM_proxy," ~", RM_proxy, "+",paste(control_vars_AM, collapse=" + ")),data=data)
  
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
  
  # Specify the linear hypothesis for AM-first stage regression, RM-Second stage regression
  sink(paste(RM_proxy,"_lmtest.txt"))
  glht_mod_AM <- glht(model = modelAM_endo, linfct = c("POST1 + POST_RPA1 = 0"))
  print(RM_proxy)
  print(summary(glht_mod_AM))
  glht_mod_RM <- glht(model = modelRM_endo, linfct = c("POST1 +POST_RPA1 = 0"))
  print(summary(glht_mod_RM))
  
  print(vif(modelAM_endo))
  print(vif(modelRM_endo))
  sink()
  
}

# Output all models in a single table
stargazer(rev(model_endo)[-c(5,3)], type = "html", report=('vc*t'),column.labels = NULL,
          se = rev(rst_endo)[-c(5,3)], 
          title = "Endogeneity Test", out = "Table7.html")

# Output all models in a single table
stargazer(rev(model_fst)[-c(5,3)], type = "html",report=('vc*t'), column.labels = NULL,
          se = rev(rst_fst)[-c(5,3)], 
          title = "First Stage", out = "Table5.html")

# Output all models in a single table
stargazer(rev(model_snd)[-c(5,3)], type = "html",report=('vc*t'), column.labels = NULL,
          se = rev(rst_snd)[-c(5,3)], 
          title = "Second Stage", out = "Table9.html")


data<-subset(data,data$RPA==1)


model_endo<-list()
model_fst<-list()
model_snd<-list()
rst_endo<-list()
rst_fst<-list()
rst_snd<-list()

for (RM_proxy in RM_proxies) {
  
  # Define control variables , "ESG",, "Zscore"
  control_vars <- c("RPA_Ctd", "NOA", "INST", "MS", "LEV", "OCF", "MTB", "SG", "ADJROA", "ADJROA_sq", "ADV","SIZE", "Big4")
  control_vars_AM <- c("POST","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL", "ADJROA", "ADJROA_sq","SIZE","BIG4","YEAR")
  control_vars_RM <- c("POST","LEV","OCF","MTB","MS","INST","CYCLE","NOA","ZSCORE","CL", "MTB", "ADJROA", "ADJROA_sq", "SIZE", "ADV","RD","YEAR")
  
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
  modelRM_endo<-lm(paste(RM_proxy,"~",AM_proxy,"+", paste(control_vars_RM, collapse=" + ")),data=data)
  
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
  modelAM_endo<-lm(paste(AM_proxy," ~  ", RM_proxy, "+",paste(control_vars_AM, collapse=" + ")),data=data)
  
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
  
  sink(paste(RM_proxy,"_vif.txt"))
  print(vif(modelAM_endo))
  print(vif(modelRM_endo))
  sink()
  
}

# Output all models in a single table
stargazer(rev(model_endo)[-c(5,3)], type = "html",report=('vc*t'), column.labels = NULL,
          se = rev(rst_endo)[-c(5,3)], 
          title = "Endogeneity Test", out = "Table6.html")

# Output all models in a single table
stargazer(rev(model_fst)[-c(5,3)], type = "html",report=('vc*t'), column.labels = NULL,
          se = rev(rst_fst)[-c(5,3)], 
          title = "First Stage", out = "Table4.html")

# Output all models in a single table
stargazer(rev(model_snd)[-c(5,3)], type = "html",report=('vc*t'), column.labels = NULL,
          se = rev(rst_snd)[-c(5,3)], 
          title = "Second Stage", out = "Table8.html")