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
data$ABSDA3<-abs(data$DA3)
data$RM<-data$ABCFO-data$ABPROD+data$ABEXP
data$RM1<-data$ABCFO+data$ABEXP
data$RM2<-data$ABPROD-data$ABEXP

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
###second step mataching


# Define your Y and X 
Y_vars <- c("ABSDA2","RM")
X_vars <- c("RM","ABSDA2")

ps_models<-list()
models <- list() # To store lm models
se_list <- list() # To store robust SEs for each model

for (Y_var in Y_vars) {
  for (X_var in X_vars) {
    if (Y_var != X_var) {
      # 1. Generate propensity scores
      ps_model <- glm(as.formula(paste0("RPA ~ ", X_var, " + LEV + OCF + MTB + ADJROA + LGTA + Age + RD + ADV + ESG + Big4 + GC + Year")), family = binomial(link = "probit"), data = data)
      # 2. Perform nearest neighbor matching
      matched_data <- matchit(as.formula(paste0("RPA ~ ", X_var, " + LEV + OCF + MTB + ADJROA + LGTA + Age + RD + ADV + ESG + Big4 + GC + Year")), data = data, method = "nearest", distance = "glm", link = "probit")
      matched_data <- match.data(matched_data)
      
      # 3. Fit a linear model on the matched data
      model_formula <- as.formula(paste0(Y_var, " ~ RPA + ", X_var, " + LEV + OCF + MTB + ADJROA + LGTA + Age + Big4 + RD + ADV + ESG + Year"))
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
stargazer::stargazer(models, type = "html", out = "PSM.html", 
                     se = se_list, title = "PSM-Regression Results with Clustered Standard Errors")


# Output all models in a single table using stargazer
stargazer::stargazer(ps_models, type = "html", out = "PSM_GLM.html", title = "PSM-GLM", model.names = FALSE, out.header = TRUE, header = FALSE)

