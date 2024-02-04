library(sandwich)
library(lmtest)
library(MASS)
library(dplyr)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("Total.csv")

# 移除含有缺失值的觀測值
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABSDA2<-abs(data$DA2)
data$ABSDA3<-abs(data$DA3)
data$RM<-data$ABCFO+data$ABEXP-data$ABPROD
data$RM1<-data$ABCFO+data$ABEXP
data$RM2<-data$ABPROD-data$ABEXP

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
data[1:8] <- lapply(data[1:8], factor)

# Apply the winsorize function to the remaining columns
data <- data %>%
  mutate(across(.cols = 9:ncol(data), .fns = ~winsorize(.)))


#Now, perform the Huber regression or any regression analysis using winsorized variables+ Year + Industry

#AM
sink("AM_Trade.txt")
# Define the different values for Y and X
Y_vars <- c("ABSDA", "ABSDA1", "ABSDA2")
X_vars <- c("RM", "RM1","RM2")

# Loop over each combination of Y and X
for (Y_var in Y_vars) {
  for (X_var in X_vars) {
    
    # Define the model formula
    formula <- as.formula(paste0(Y_var, " ~ RPA + ", X_var, 
                                 " + LEV + OCF + MTB + ADJROA + LGTA + Age_Trade + Big4 + RD + ADV + ESG + GC + Year"))
    
    # Fit the model
    model <- lm(formula, data = data)
    
    # Print summary
    print(paste0("Model: ", Y_var, " ~ ", X_var))
    print(summary(model))
    
    # Perform coeftest with vcovHC
    print(coeftest(model, vcov = vcovHC(model)))
    
    # Perform coeftest with vcovCL
    print(coeftest(model, vcov = vcovCL(model, cluster = ~Key)))
    
    # Optionally, you can save the model results or coefficients into a list or data frame if needed
  }
}
sink()


# RM
sink("RM_Trade.txt")
# Define the different values for Y and X
X_vars <- c("ABSDA", "ABSDA1", "ABSDA2")
Y_vars <- c("RM", "RM1","RM2","ABCFO","ABEXP","ABPROD")

# Loop over each combination of Y and X
for (Y_var in Y_vars) {
  for (X_var in X_vars) {
    
    # Define the model formula
    formula <- as.formula(paste0(Y_var, " ~ RPA + ", X_var, 
                                 " + LEV + OCF + MTB + ADJROA + LGTA + Age_Trade + Big4 + RD + ADV + ESG + GC + Year"))
    
    # Fit the model
    model <- lm(formula, data = data)
    
    # Print summary
    print(paste0("Model: ", Y_var, " ~ ", X_var))
    print(summary(model))
    
    # Perform coeftest with vcovHC
    print(coeftest(model, vcov = vcovHC(model)))
    
    # Perform coeftest with vcovCL
    print(coeftest(model, vcov = vcovCL(model, cluster = ~Key)))
    
    # Optionally, you can save the model results or coefficients into a list or data frame if needed
  }
}
sink()

