library(sandwich)
library(lmtest)
library(MASS)
library(dplyr)
library(stargazer)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("Total.csv")

# 移除含有缺失值的觀測值
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABSDA2<-abs(data$DA2)
data$RM<-data$ABCFO+data$ABEXP-data$ABPROD
data$RM1<-data$ABCFO+data$ABEXP
data$RM2<-data$ABEXP-data$ABPROD

#data<-subset(data,data$DA2<0)

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

data$ADJROA_sq<-data$ADJROA*data$ADJROA

#Now, perform the Huber regression or any regression analysis using winsorized variables+ Year + Industry

#AM
#sink("AM_N.txt")
# Define the different values for Y and X

models <- list() # To store lm models
se_list <- list() # To store robust SEs for each model
Y_vars <- c("ABSDA","ABSDA1","ABSDA2") # Updated Y_vars to distinguish between positive and negative DA2
X_vars <- c("RM")

models <- list() # To store lm models
se_list <- list() # To store robust SEs for each model

for (Y_var in Y_vars) {
  for (X_var in X_vars) {
    # Define the model formula
    formula <- as.formula(paste0(gsub("_pos|_neg", "", Y_var), " ~ RPA_Ctd  * ", X_var, " + LEV + OCF + MTB + ADJROA + ADJROA_sq + LGTA + Age + RD + ADV + ESG + Big4 + Year"))
    
    # Filter data based on the condition (if applicable)
    if (Y_var == "_pos") {
      temp_data <- data[data$DA2 > 0, ]
    } else if (Y_var == "DA2_neg") {
      temp_data <- data[data$DA2 < 0, ]
    } else {
      temp_data <- data
    }
    
    # Fit the model with the filtered data
    model <- lm(formula, data = temp_data)
    
    # Calculate clustered standard errors
    robust_se <- sqrt(diag(vcovCL(model, type = "HC0", cluster = ~Key)))
    
    # Store the model and its robust SE
    models[[paste0(Y_var, "_", X_var)]] <- model
    se_list[[paste0(Y_var, "_", X_var)]] <- robust_se
  }
}

# Output all models in a single table
stargazer(models, type = "html", 
          se = se_list, 
          title = "AM-Regression Results with Clustered Standard Errors", out = "AMwoI.html")



# RM
#sink("RM.txt")
# Define the different values for Y and X
# Assuming 'data' is your dataframe and 'Key' is your clustering variable
models <- list() # To store lm models
se_list <- list() # To store robust SEs for each model

X_vars <- c("ABSDA","ABSDA1","ABSDA2")
Y_vars <- c("ABCFO","ABPROD","ABEXP","RM","RM1","RM2")

for (Y_var in Y_vars) {
  for (X_var in X_vars) {
    # Define the model formula+ ", X_var, "
    formula <- as.formula(paste0(Y_var, " ~ RPA_Ctd  * ", X_var, " + LEV + OCF + MTB + ADJROA + ADJROA_sq + LGTA + Age + RD + ADV + ESG + Big4 + Year"))
    
    # Fit the model
    model <- lm(formula, data = data)
    
    # Calculate clustered standard errors
    robust_se <- sqrt(diag(vcovCL(model, type = "HC0", cluster = ~Key)))
    
    # Store the model and its robust SE
    models[[paste0(Y_var, "_", X_var)]] <- model
    se_list[[paste0(Y_var, "_", X_var)]] <- robust_se
  }
}

# Assuming you want to output all models in a single table
stargazer(models, type = "html", 
          se = se_list, 
          title = "RM-Regression Results with Clustered Standard Errors", out = "RMwoI.html")

