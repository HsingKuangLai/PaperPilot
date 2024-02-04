library(sandwich)
library(lmtest)
library(MASS)
library(Matching)
library(rgenoud)
library(MatchIt)
library(dplyr)
# read CSV
data <- read.csv("Total.csv", na.strings = "#N/A")
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABSDA2<-abs(data$DA2)
data$ABSDA3<-abs(data$DA3)
data$RM<-data$ABCFO+data$ABEXP-data$ABPROD
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

sink("PSM_AM.txt")
# Define your Y and X variables
Y_vars <- c("ABSDA", "ABSDA1", "ABSDA2")
X_vars <- c("RM", "RM1", "RM2")

# Loop through each Y and X combination
for (Y_var in Y_vars) {
  for (X_var in X_vars) {
    print(paste0("Model: ", Y_var, " ~ ", X_var))
    # 1. Generate propensity scores
    ps_model <- glm(RPA ~ get(X_var) + LEV + OCF + MTB  + ADJROA + LGTA + Age + RD + ADV + ESG + Big4 + GC + Year, data = data)
    data$propensity_score <- predict(ps_model, type = "response")
    
    # 2. Perform nearest neighbor matching
    matched_data <- matchit(RPA ~ get(X_var) + LEV + OCF + MTB  + ADJROA + LGTA + Age + RD + ADV + ESG + Big4 + GC + Year, data = data, link = "probit", method = "nearest", distance = "glm")
    matched_data <- match.data(matched_data)
    
    # 3. Fit a linear model on the matched data
    model_formula <- as.formula(paste0(Y_var, " ~ RPA + ", X_var, " + LEV + OCF + MTB + ADJROA + LGTA + Age + Big4 + RD + ADV + ESG + Year"))
    model <- lm(model_formula, data = matched_data)
    
    # 4. Print summary and perform coeftest
    print(summary(model))
    print(coeftest(model, vcov = vcovHC(model)))
    print(coeftest(model, vcov = vcovCL(model, cluster = ~Key)))
  }
}
sink()

sink("PSM_RM.txt")
# Define your Y and X variables
X_vars <- c("ABSDA", "ABSDA1", "ABSDA2")
Y_vars <- c("RM", "RM1", "RM2","ABCFO","ABEXP","ABPROD")

# Loop through each Y and X combination
for (Y_var in Y_vars) {
  for (X_var in X_vars) {
    print(paste0("Model: ", Y_var, " ~ ", X_var))
    # 1. Generate propensity scores
    ps_model <- glm(RPA ~ get(X_var) + LEV + OCF + MTB  + ADJROA + LGTA + Age + RD + ADV + ESG + Big4 + GC + Year, data = data)
    data$propensity_score <- predict(ps_model, type = "response")
    
    # 2. Perform nearest neighbor matching
    matched_data <- matchit(RPA ~ get(X_var) + LEV + OCF + MTB  + ADJROA + LGTA + Age + RD + ADV + ESG + Big4 + GC + Year, data = data, link = "probit", method = "nearest", distance = "glm")
    matched_data <- match.data(matched_data)
    
    # 3. Fit a linear model on the matched data
    model_formula <- as.formula(paste0(Y_var, " ~ RPA + ", X_var, " + LEV + OCF + MTB + ADJROA + LGTA + Age + Big4 + RD + ADV + ESG + Year"))
    model <- lm(model_formula, data = matched_data)
    
    # 4. Print summary and perform coeftest
    print(summary(model))
    print(coeftest(model, vcov = vcovHC(model)))
    print(coeftest(model, vcov = vcovCL(model, cluster = ~Key)))
  }
}
sink()



