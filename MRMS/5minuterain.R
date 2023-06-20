# Load necessary packages
library(lubridate)
library(dplyr)
library(readr)

# Load your data
data <- read_csv('MRMS_1min.csv')

# Make sure datetime is in the right format
data$datetime <- ymd_hms(data$datetime)

# Group by 5-minute intervals and sum the values
data_5min <- data %>%
  group_by(datetime_5min = floor_date(datetime, "5 minutes")) %>%
  select(-datetime) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

write.csv(data_5min, "MRMS_5min.csv")

#######
data1 <- read_csv("MRMS_5min.csv")
data1$datetime <- mdy_hm(data1$datetime)
