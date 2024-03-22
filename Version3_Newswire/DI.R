library(dplyr)
#data <- read.csv("data_withNA.csv", na.strings = "#N/A")
#data <- na.omit(data)
#write.csv(data,"data.csv")

# Load Data
data <- read.csv("data.csv")

# Calculate market share and code frequency
data <- data %>%
  group_by(IndustryY) %>%
  mutate(TotalSales = sum(S), # Calculate total sales for each industry-year
         MarketShare = S / TotalSales * 100) %>% # Calculate market share as percentage
  ungroup() %>%
  add_count(IndustryY, name = "IndustryY_Count") # Count the frequency of each Code and add it as a new column

data<-subset(data,data$IndustryY_Count>=15)

# calculation
data$LEV <- data$Debt/data$Asset
data$OCF <- data$CFO/data$Asset
data$MTB <- data$MV/data$Equity
data$INST <- data$INST_1
data$CYCLE <- data$CYCLE_1
data$NOA <- (data$Asset-data$Cash-data$Debt+data$SLTD)/data$Asset_1
##Zscor: applying CL first
data$ZSCORE <- 1.2*(data$CA-data$CL)/data$Asset + 1.4*(data$RE/data$Asset) + 3.3*(data$EBIT/data$Asset) + 0.6*(data$MV/data$Debt) + data$S/data$Asset
##coverage
data$CL <- (data$CL-data$STD)/data$Asset_1
data$LGTA <- log(data$Asset)
data$ROA <- data$NI_Ctd/data$Asset_1
data$ADV<-data$ADV/data$S
data$RD<-data$RD/data$S

# MS
data <- data %>% 
  group_by(IndustryY) %>%
  mutate(TotalSales = sum(S), # Calculate total sales for each industry-year
         MS = S / TotalSales * 100) # Calculate market share as percentage

# ADJROA
data <- data %>%
  group_by(IndustryY) %>%
  mutate(
    Median_ROA = median(ROA, na.rm = TRUE), # Calculate median of ROA for each Industry-Year group
    ADJROA = ROA - Median_ROA # Subtract median ROA from ROA for each row
  ) %>%
  ungroup() # Ungroup the data frame


# EM proxy calculation
#Jones Model

data$AC<-(data$NI_Ctd-data$CFO)/data$Asset_1
data$A1<-1/data$Asset_1
data$A2<-(data$S-data$S_1)/data$Asset_1
data$A3<-data$PPE/data$Asset_1
data$A4<-data$NI_Ctd/data$Asset_1

data$OCF<-data$CFO/data$Asset_1
data$PROD<-(data$COGS + data$INV - data$INV_1 )/data$Asset_1
data$OPCOST <- (data$OPCOST + data$INV - data$INV_1 )/data$Asset_1
data$EXP<-data$EXP/data$Asset_1
data$R1<-1/data$Asset_1
data$R2<-data$S/data$Asset_1
data$R3<-(data$S-data$S_1)/data$Asset_1
data$R4<-(data$S_1-data$S_2)/data$Asset_1
data$R5<-data$S_1/data$Asset_1

unique_industries <- unique(data$Industry)
unique_years <- unique(data$YEAR)
all_industries_data <- NULL

for(industry in unique_industries) {
  # Subset data for the current industry
  industry_data <- data %>% filter(Industry == industry)
  unique_years <- unique(industry_data$YEAR)
  
  for(year in unique_years) {
    # Subset data for the current year within the industry
    year_data <- industry_data %>% filter(YEAR == year)
    
    #year_data <- year_data %>%
    #mutate(across(.cols = c("AC","A1", "A21","A2","A3","A4","A6","CFO","PROD","EXP","R1","R2","R3","R4","R5"), .fns = ~winsorize(.)))
    
    # Perform your Jones model and Roychowdhury measures calculations here, adjusted for year_data
    # Example for a simplified Jones model
    jones_model <- lm(AC ~ A1 + A2 + A3, data = year_data)
    year_data$DA <- residuals(jones_model)
    
    jones_model1 <- lm(AC ~ A1 + A2 + A3 + A4, data = year_data)
    year_data$DA1 <- residuals(jones_model1)
    
    # Example for Roychowdhury measures
    model_cfo <- lm(OCF ~ R1 + R2 + R3, data = year_data)
    year_data$ABCFO <- residuals(model_cfo)
    
    model_prod <- lm(PROD ~ R1 + R2 + R3 + R4, data = year_data)
    year_data$ABPROD <- residuals(model_prod)
    
    model_opcost <- lm(OPCOST ~ R1 + R2 + R3 + R4, data = year_data)
    year_data$ABOPCOST <- residuals(model_opcost)
    
    model_exp <- lm(EXP ~ R1 + R5, data = year_data)
    year_data$ABEXP <- residuals(model_exp)
    
    # Append the modified data frame to the all_industries_data dataframe
    if(is.null(all_industries_data)) {
      all_industries_data <- year_data
    } else {
      all_industries_data <- rbind(all_industries_data, year_data)
    }
    
    # Save regression results to text files, adjusted to include year in the filename
    file_name <- paste0("Industry_", as.character(industry), "_Year_", as.character(year), "_Regression_Results.txt")
    sink(file_name)
    cat("Jones Model for Year ", year, ":\n")
    print(summary(jones_model))
    cat("\nJones Model 1 for Year ", year, ":\n")
    print(summary(jones_model1))
    cat("\nCFO Model for Year ", year, ":\n")
    print(summary(model_cfo))
    cat("\nPROD Model for Year ", year, ":\n")
    print(summary(model_prod))
    cat("\nOPCOST Model for Year ", year, ":\n")
    print(summary(model_opcost))
    cat("\nEXP Model for Year ", year, ":\n")
    print(summary(model_exp))
    sink()
  }
}

# Calculate market share and code frequency
all_industries_data <- all_industries_data %>%
  group_by(Code) %>%
  add_count(Code, name = "Code_Count") # Count the frequency of each Code and add it as a new column

write.csv(all_industries_data,"total.csv")

