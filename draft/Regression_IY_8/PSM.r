library(sandwich)
library(lmtest)
library(MASS)
library(Matching)
library(rgenoud)
library(MatchIt)
library(dplyr)
library(knitr)
library(kableExtra)
# read CSV
data <- read.csv("Total.csv", na.strings = "#N/A")
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABSDA2<-abs(data$DA2)
data$RM<-data$ABCFO-data$ABPROD+data$ABEXP
data$RM1<-data$ABCFO+data$ABEXP
data$RM2<--data$ABPROD+data$ABEXP

######fisrt step######################### winsorizing

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

# Factorize the first 6 columns
data[1:8] <- lapply(data[1:8], factor)

# Apply the winsorize function to the remaining columns
data <- data %>%
  mutate(across(.cols = 9:ncol(data), .fns = ~winsorize(.)))

data$RPA<-as.double(data$RPA)-1
data$RPA_Ctd<-as.double(data$RPA_Ctd)-1
###second step mataching
data$ADJROA_sq<-data$ADJROA*data$ADJROA

# Define your Y and X 
Y_vars <- c("ABSDA","ABSDA1","ABSDA2","ABCFO","ABPROD","ABEXP","RM","RM1","RM2")
X_vars <- c("ABSDA","ABSDA1","ABSDA2","RM")

ps_models<-list()
models <- list() # To store lm models
se_list <- list() # To store robust SEs for each model

for (Y_var in Y_vars) {
  for (X_var in X_vars) {
    if (substr(Y_var,1,3)!=substr(X_var,1,3)) {
      # 1. Generate propensity scores
      ps_model <- glm(as.formula(paste0("RPA_Ctd ~ ", X_var, " + LEV + OCF + MTB + ADJROA + ADJROA_sq + LGTA + Age + RD + ADV + ESG  + Big4 + Year")), data = data)
      # 2. Perform nearest neighbor matching
      matched_data <- matchit(as.formula(paste0("RPA_Ctd ~ ", X_var, " + LEV + OCF + MTB + ADJROA  + ADJROA_sq  + LGTA + Age  + RD + ADV + ESG  + Big4 + Year")), data = data,link="logit", method = "nearest",distance = "glm")
      matched_data <- match.data(matched_data)
      
      # 3. Fit a linear model on the matched data
      model_formula <- as.formula(paste0(Y_var, " ~ RPA_Ctd * ", X_var, " + LEV + OCF + MTB + ADJROA + ADJROA_sq + LGTA + Age  + RD + ADV + ESG  + Big4 + Year "))
      model <- lm(model_formula, data = matched_data)
      
      # Calculate clustered standard errors
      robust_se <- sqrt(diag(vcovCL(model, type = "HC0", cluster = ~Key)))
      
      # Store the model, its robust SE
      models[[paste0(Y_var, "_", X_var)]] <- model
      se_list[[paste0(Y_var, "_", X_var)]] <- robust_se
      ps_models[[paste0(Y_var, "_", X_var)]]<-ps_model
    }
  }
}

# Output all models in a single table using stargazer
stargazer::stargazer(models, type = "html", out = "PSM.html", 
                     se = se_list, title = "PSM-Regression Results with Clustered Standard Errors")


# Calculate McFadden's pseudo R^2 for each propensity score model
pseudo_r_squared <- sapply(ps_models, function(model) {
  logLik_model <- logLik(model)
  # Create and fit the null model
  null_model <- update(model, . ~ 1)
  logLik_null <- logLik(null_model)
  
  # Calculate McFadden's pseudo R^2
  r_squared <- 1 - (logLik_model / logLik_null)
  return(as.numeric(r_squared))
})

print(pseudo_r_squared)


# Output all models in a single table using stargazer
stargazer::stargazer(ps_models, type = "html", out = "PSM_GLM.html", title = "PSM-GLM", model.names = FALSE, out.header = TRUE, header = FALSE)

