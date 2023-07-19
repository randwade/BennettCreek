# Load necessary packages
library(lubridate)
library(dplyr)
library(readr)

# Load your data
setwd("~/R/Bennett/MRMS/combine")
data <- read_csv('MRMS_1min.csv')

# Make sure datetime is in the right format
data$datetime <- ymd_hms(data$datetime)

# Group by 10-minute intervals and sum the values
data_10min <- data %>%
  group_by(datetime_10min = floor_date(datetime, "10 minutes")) %>%
  select(-datetime) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  mutate_at(vars(-datetime_10min), ~ . / 60)


write.csv(data_10min, "new_MRMS_10min.csv")

#######
data1 <- read_csv("MRMS_5min.csv")
data1$datetime <- mdy_hm(data1$datetime)

