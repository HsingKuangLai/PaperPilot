# Load necessary libraries
library(sampleSelection)
library(dplyr)
library(pscl)

# Load the data
data <- read.csv("data_IMR.csv")

# Define the formula for the probit model
formula <- POST ~ LNAT + LEV + CAPINT + MB + LOSS + DIV + RDINT + ADVINT + ANALYST + factor(YEAR)

# Fit the probit model
probit_model <- glm(formula, data = data, family = binomial(link = "probit"))

summary(probit_model)
# Calculate the pseudo R-squared
pseudo_r2 <- pR2(probit_model)

# Calculate the linear prediction
data$linear_prediction <- predict(probit_model, type = "link")

# Calculate the Inverse Mills Ratio (IMR)
data$IMR <- dnorm(data$linear_prediction) / pnorm(data$linear_prediction)

#write.csv(data,"data_IMR_complete.csv")

