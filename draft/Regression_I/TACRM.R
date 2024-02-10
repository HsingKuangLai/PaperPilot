library(dplyr)
library(stargazer)
library(gdata)
# 讀取 CSV 檔案，將 "#N/A" 轉換為真正的 NA（缺失值）
data <- read.csv("clean.csv")


# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

# Assuming 'data' is your dataframe
data$Industry <- as.factor(data$Industry)
data$Year <- as.factor(data$Year)


# Initialize columns in 'data' for the abnormal measures
data$DA <- NA  # Discretionary Accruals for Jones model
data$DA1 <- NA
data$DA2 <- NA
data$DA3 <- NA
data$ABCFO <- NA  # Abnormal Cash Flow from Operations
data$ABPROD <- NA  # Abnormal Production Costs
data$ABEXP <- NA  # Abnormal Discretionary Expenses

#Jones/MJONES/MJONES_ROA/MJONES_BMCFO
data$AC<-(data$NI_Ctd-data$OCF)/data$Asset_1
data$A1<-1/data$Asset_1
data$A21<-(data$S-data$S_1)/data$Asset_1
data$A2<-(data$S-data$S_1-(data$AR-data$AR_1))/data$Asset_1
data$A3<-data$PPE/data$Asset_1
data$A4<-data$NI_Ctd/data$Asset_1
data$A5<-data$BM
data$A6<-data$OCF/data$Asset_1
data$ROA<-data$NI_Ctd/data$Asset_1



# Assuming your dataframe is named 'data' and it has columns 'ROA', 'Industry', and 'Year'
data <- data %>%
  group_by(Industry) %>%
  mutate(
    Median_ROA = median(ROA, na.rm = TRUE), # Calculate median of ROA for each Industry-Year group
    ADJROA = ROA - Median_ROA # Subtract median ROA from ROA for each row
  ) %>%
  ungroup() # Ungroup the data frame


data$CFO<-data$OCF/data$Asset_1
data$PROD<-data$PROD/data$Asset_1
data$EXP<-data$EXP/data$Asset_1
data$R1<-1/data$Asset_1
data$R2<-data$S/data$Asset_1
data$R3<-(data$S-data$S_1)/data$Asset_1
data$R4<-(data$S_1-data$S_2)/data$Asset_1
data$R5<-data$S_1/data$Asset_1

# Loop through each industry to perform calculations
unique_industries <- unique(data$Industry)
all_industries_data<-NULL

for(industry in unique_industries) {
  # Subset data for the current industry
  industry_data <- data %>% filter(Industry == industry)
  
  # Perform your Jones model and Roychowdhury measures calculations here
  # Example for a simplified Jones model
  jones_model <- lm(AC ~ A1 + A21 + A3 , data = industry_data)
  industry_data$DA <- residuals(jones_model)
  
  jones_model1 <- lm(AC ~ A1 + A2 + A3 , data = industry_data)
  industry_data$DA1 <- residuals(jones_model1)
  
  jones_model2 <- lm(AC ~ A1 + A2 + A3 + A4 , data = industry_data)
  industry_data$DA2 <- residuals(jones_model2)
  
  jones_model3 <- lm(AC ~ A1 + A2 + A3 + A4 + A5 + A6 , data = industry_data)
  industry_data$DA3 <- residuals(jones_model3)
  
  # Example for Roychowdhury measures
  model_cfo <- lm(CFO ~ R1 + R2 + R3, data = industry_data)
  industry_data$ABCFO <- residuals(model_cfo)
  
  model_prod <- lm(PROD ~ R1 + R2 + R3 + R4, data = industry_data)
  industry_data$ABPROD <- residuals(model_prod)
  
  model_exp <- lm(EXP ~ R1 + R5, data = industry_data)
  industry_data$ABEXP <- residuals(model_exp)
  
  # Append the modified data frame to the all_industries_data dataframe
  if(is.null(all_industries_data)) {
    all_industries_data <- industry_data
  } else {
    all_industries_data <- rbind(all_industries_data, industry_data)
  }
}

rawdata <- read.csv("data.csv")
# Ensure both data frames have 'Key' as a common column and it's of the same type
# It's a good practice to ensure that 'Key' columns are of the same type (e.g., character or factor)

# Merge the TAC data with rawdata based on 'Key'
Total <- merge(rawdata, all_industries_data, by = "Key")

# Write the merged data to a new CSV file
write.csv(Total, "Total.csv", row.names = FALSE)