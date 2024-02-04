library(sandwich)
library(lmtest)
library(MASS)
library(Matching)
library(rgenoud)
library(MatchIt)
# read CSV
data <- read.csv("Total.csv", na.strings = "#N/A")
data$ABSDA<-abs(data$DA)
data$ABSDA1<-abs(data$DA1)
data$ABSDA2<-abs(data$DA2)
data$ABSDA3<-abs(data$DA3)
data$RM<-data$ABCFO+data$ABEXP-data$ABPROD
data$RM1<-data$ABCFO+data$ABEXP

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

library(MatchIt)
## generate psm score by glm 
ps_model <- glm(RPA ~ (RM + LEV + OCF + MTB  + ADJROA + LGTA + Age_Trade + RD + ADV + ESG + Big4 + GC) + Year, data = data)
data$propensity_score <- predict(ps_model)

# matching parameter method=nn, distance=glm, link=probit, caliper=NULL
matched_data<- matchit( RPA ~ (RM + LEV + OCF + MTB  + ADJROA + LGTA + Age_Trade + RD + ADV + ESG + Big4 + GC) + Year , data = data, link="probit", method = "nearest",distance = "glm")
data <- match.data(matched_data)


#AM
sink("PSM_AM_Trade.txt")

model <- (lm((ABSDA2) ~ RPA  + (RM + LEV + OCF + MTB  + ADJROA  + LGTA + Age_Trade + Big4 + RD + ADV + ESG) + Year , data = data))
summary(model)
coeftest(model, vcov = vcovHC(model))
coeftest(model, vcov = vcovCL(model,cluster = ~Key))

sink()


###
sink("PSM_RM_Trade.txt")
X_vars <- c("ABSDA2")
Y_vars <- c("RM", "RM1","ABCFO","ABEXP","ABPROD")

# Loop over each combination of Y and X
for (Y_var in Y_vars) {
  for (X_var in X_vars) {
    
    # Define the model formula
    formula <- as.formula(paste0(Y_var, " ~ RPA + ", X_var, 
                                 " + LEV + OCF + MTB + ADJROA + LGTA + Age_Trade + Big4 + RD + ADV + ESG + Year"))
    
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





