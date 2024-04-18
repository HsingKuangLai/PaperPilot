# Load Data
library(dplyr)
library(tidyr)
rawdata <- read.csv("AQfactor.csv")
rawdata <- rawdata %>%
  arrange(KEY)

fstdata<-read.csv("FirstEq.csv")
fstdata<-fstdata[,c("KEY","AQ","A_1")]

alldata<-full_join(fstdata,rawdata,by="KEY")

# Scaling

alldata$AvgA<-0.5*(alldata$A+alldata$A_1)
alldata$CFO<-alldata$CFO/alldata$AvgA
alldata$S<-alldata$S/alldata$AvgA
alldata$NI_Ctd<-alldata$NI_Ctd/alldata$AvgA

# Lead and Lag
alldata <- alldata %>%
  group_by(Code) %>%
  mutate(
    S_1 = lag(S, 1),
    S_2 = lag(S, 2),
    S_3 = lag(S, 3),
    S_4 = lag(S, 4),
    AQ_1 = lag(AQ, 1),
    AQ_2 = lag(AQ, 2),
    AQ_3 = lag(AQ, 3),
    AQ_4 = lag(AQ, 4),
    CFO_1 = lag(CFO, 1),
    CFO_2 = lag(CFO, 2),
    CFO_3 = lag(CFO, 3),
    CFO_4 = lag(CFO,4),
    NI_Ctd_1 = lag (NI_Ctd,1),
    NI_Ctd_2 = lag (NI_Ctd,2),
    NI_Ctd_3 = lag (NI_Ctd,3),
    NI_Ctd_4 = lag (NI_Ctd,4)
  ) %>%
  ungroup()


# DAQ, IAQ calculation

data<-na.omit(alldata)

data$LGTA<-log(data$A)
data$LGCYCLE<-log(data$Cycle+1)

# Assuming data is your data frame
data <- data %>%
  rowwise() %>% # Apply functions row by row
  mutate(
    sigAQ = sd(c(AQ, AQ_1, AQ_2,AQ_3,AQ_4)),
    sigCFO = sd(c(CFO, CFO_1, CFO_2,CFO_3,CFO_4)),
    sigS = sd(c(S, S_1, S_2,S_3,S_4)),
    NegNI = sum(c(NI_Ctd, NI_Ctd_1, NI_Ctd_2,NI_Ctd_3,NI_Ctd_4) < 0)
    ) %>% # Calculate standard deviation of A, B, C for each row
  ungroup() # Remove the rowwise grouping


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
    IAQ_model <- lm(sigAQ ~ LGTA + sigCFO + sigS + LGCYCLE + NegNI , data = year_data)
    year_data$DAQ <- residuals(IAQ_model)
    year_data$IAQ <- predict(IAQ_model)
    
 
    
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
    print(summary(IAQ_model))
    #sink()
  }
}


write.csv(all_industries_data,"SecondEq.csv")

