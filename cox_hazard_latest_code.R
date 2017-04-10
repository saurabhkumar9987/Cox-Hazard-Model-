library(dplyr)
library(rose)
library(caret)
library(cluster)
library(survival)
library(lubridate)

getwd()
mdata_or <- read.csv("final_master_data_1.csv", stringsAsFactors = FALSE)
summary(mdata_or)

mdata_or <- mdata_or %>%
  filter(TotalWage != 'NULL',
         TotalHours != 'NULL',
         TotalSales != 'NULL',
         Tenure_in_days > 0)

colnames(mdata_or)[1] <- "EmployeeKey"

mdata <- mdata_or
head(mdata)

mdata$TotalSales <- as.numeric(mdata$TotalSales)
mdata$TotalHours <- as.numeric(mdata$TotalHours)
mdata$TotalWage <- as.numeric(mdata$TotalWage)
mdata$Age <- as.integer(mdata$Age)
mdata$Stylist_is_Active <- as.factor(mdata$Stylist_is_Active)
mdata$JobTitle <- as.factor(mdata$JobTitle)
mdata$Division <- as.factor(mdata$Division)
mdata$OrgCountryCode <- as.factor(mdata$OrgCountryCode)
mdata$ReHireFlag <- as.factor(mdata$ReHireFlag)
mdata$DeductionFlag <- as.factor(mdata$DeductionFlag)


mdata$HireDate <- as.Date(mdata$HireDate)


class(mdata$HireDate)
mdata$TerminationDate <- as.Date(mdata$TerminationDate)

mdata$HireDate_New  <- as.Date(mdy(mdata$HireDate, "%m/%d/%Y %H:%M:%S"))
mdata$HireDate_New  <- NULL 

mdata <- mdata[!mdata$Division == "Trade Secret",]


subset_data <- mdata %>%
  filter(Age > 0, Tenure_in_days > 1, TotalHours > 1, TotalSales > 0) %>%
  mutate(HourlySales = as.numeric(TotalSales)/as.numeric(TotalHours),
         HourlyWage = as.numeric(TotalWage)/as.numeric(TotalHours),
         PctTopStylist = as.numeric(TopStylist)/as.numeric(Tenure_in_days),
         Churn = ifelse(Stylist_is_Active == 0, 1, 0)) %>%
  select(EmployeeKey, Age, JobTitle, Division, 
         OrgCountryCode, HireDate, NumberOfTerminations,
         TotalGap, ReHireFlag, Tenure_in_days,
         DeductionFlag, HourlySales, HourlyWage, PctTopStylist, Churn, TerminationDate) %>%
  filter(PctTopStylist <= 100, HourlySales <= 58.17, HourlyWage <= 19.38)

subset_data$Churn <- as.factor(subset_data$Churn)

summary(subset_data)

subset2015 <- subset_data %>%
  filter(HireDate <= '2015-01-01') %>%
  filter(TerminationDate > '2015-01-01' | is.na(TerminationDate))

subset2015$churn2015 <- ifelse(is.na(subset2015$TerminationDate), 0, 
                             ifelse(format(subset2015$TerminationDate, "%Y")=='2015', 1, 0))
subset2015$churn2015 <- as.factor(subset2015$churn2015)

subset2015$week <- ifelse(subset2015$churn2015 == 0, 52, 
                          format(subset2015$TerminationDate, "%U"))

subset2015$week <- as.numeric(subset2015$week)
subset2015$churn2015 <- as.numeric(subset2015$churn2015)


scaled_data <- subset2015
scaled_data$Age <- scale(scaled_data$Age, center = TRUE)
scaled_data$NumberOfTerminations <- scale(scaled_data$NumberOfTerminations, center = TRUE)
scaled_data$TotalGap <- scale(scaled_data$TotalGap, center = TRUE)
scaled_data$Tenure_in_days <- scale(scaled_data$Tenure_in_days, center = TRUE)
scaled_data$HourlySales <- scale(scaled_data$HourlySales, center = TRUE)
scaled_data$HourlyWage <- scale(scaled_data$HourlyWage, center = TRUE)
scaled_data$PctTopStylist <- scale(scaled_data$PctTopStylist, center = TRUE)
scaled_data$week <- as.numeric(scaled_data$week)
scaled_data$churn2015 <- as.numeric(scaled_data$churn2015)

model.1 <- coxph(Surv(week, churn2015) ~ Age + Division + HourlyWage + HourlySales, data = subset2015)

covs <- data.frame(Age = 40, 
                   Division = "MasterCuts", 
                   HourlyWage = 8,
                   HourlySales = 20)

summary(survfit(model.1, individual = TRUE, newdata = covs, type = "aalen"))

Train <- createDataPartition(scaled_data$Churn, p=0.8, list=FALSE)
training <- scaled_data[ Train, ]
testing <- scaled_data[ -Train, ]

train_control <- trainControl(method = "cv", number = 5)
