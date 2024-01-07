# Import
data <- read.csv("data.csv", na.strings = "#N/A")

# Remove NA
data <- na.omit(data)
data$Year <- factor(data$Year)
data$KY <- factor(data$KY)
data$Finance <- factor(data$Finance)
data$Industry <- factor(data$Industry)
data$NB<-as.numeric(data$NB)
data$PBRatio <- data$Price/data_PSM$BVE
data <- na.omit(data)

# 假設你的資料框架名為 data
ps_model <- glm(RPA ~ NB + Year + Industry, data = data)

# 提取傾向分數
data$propensity_score <- predict(ps_model, type = "response")

# 執行傾向分數匹配
matched_data <- matchit(RPA ~ NB  + Year + Industry   , data = data, method = "nearest")

# 從匹配物件中提取匹配後的資料框架
data_PSM <- match.data(matched_data)

# Winsorize each continuous variable

# Define a function for winsorizing
winsorize <- function(x) {
  p1 <- quantile(x, probs = 0.01)  # 1st percentile value
  p99 <- quantile(x, probs = 0.99)  # 99th percentile value
  x[x < p1] <- p1  # Replace values below 1st percentile
  x[x > p99] <- p99  # Replace values above 99th percentile
  return(x)
}

data_PSM$NB <- winsorize(data_PSM$NB)
data_PSM$PBRatio <- winsorize(data_PSM$PBRatio)


#!!!跟finance有交互作用RRRR
matched_regression1 <- lm( PBRatio ~ NB + Year + Industry * RPA   , data = data_PSM)
summary(matched_regression1)

matched_regression2 <- lm( PBRatio ~  NB + Year + Industry + RPA    , data = data_PSM)
summary(matched_regression2)


