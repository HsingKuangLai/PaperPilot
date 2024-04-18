# Load Data
library(dplyr)
library(tidyr)
rawdata <- read.csv("AQfactor.csv")
rawdata <- rawdata %>%
  arrange(KEY)

# Lead and Lag
rawdata <- rawdata %>%
  group_by(Code) %>%
  mutate(
    A_1 = lag(A, 1),
    S_1 = lag(S, 1),
    CFO_pre = lag(CFO, 1),
    CFO_post = lead(CFO, 1),
    Cash_1=lag(Cash,1),
    CA_1=lag(CA,1),
    CL_1=lag(CL,1),
    STD_1=lag(STD,1)
  ) %>%
  ungroup()


# AQ calculation

data<-na.omit(rawdata)

data$AvgA<-0.5*(data$A+data$A_1)
data$deltaWC<-((data$CA-data$CA_1)-(data$CL-data$CL_1)-(data$Cash-data$Cash_1)+(data$STD-data$STD_1))/data$AvgA
data$CFO_pre<-data$CFO_pre/data$AvgA
data$CFO<-data$CFO/data$AvgA
data$CFO_post<-data$CFO_post/data$AvgA
data$deltaS<-(data$S-data$S_1)/data$AvgA
data$PPE<-data$PPE/data$AvgA

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
    
    # Perform your Jones model and Roychowdhury measures calculations here, adjusted for year_data
    # Example for a simplified Jones model
    MDD_model <- lm(deltaWC ~ CFO_pre + CFO + CFO_post + deltaS + PPE , data = year_data)
    year_data$AQ <- residuals(MDD_model)
 
    
    # Append the modified data frame to the all_industries_data dataframe
    if(is.null(all_industries_data)) {
      all_industries_data <- year_data
    } else {
      all_industries_data <- rbind(all_industries_data, year_data)
    }
    
    # Save regression results to text files, adjusted to include year in the filename
    file_name <- paste0("Industry_", as.character(industry), "_Year_", as.character(year), "_Regression_Results.txt")
    #sink(file_name)
    cat("Jones Model for Year ", year, ":\n")
    print(summary(MDD_model))
    #sink()
  }
}


write.csv(all_industries_data,"FirstEq.csv")

