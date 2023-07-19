# Load necessary packages
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Load the coordinates
setwd("~/R/Bennett/MRMS/combine")
coords <- read_csv('bucket_coords.csv')

# Define a list of your file paths
file_list <- list.files(path = "~/R/Bennett/MRMS/combine/bucket", pattern = "*.csv", full.names = TRUE)

# A function to process each file
process_file <- function(file) {
  data <- read_csv(file)
  
  # Check units and convert if necessary
  if("P_in" %in% colnames(data)) {
    data$P_in <- data$P_in * 25.4 # convert inches to mm
    names(data)[names(data) == "P_in"] <- "P_mm" # rename column
  }
  
  # Convert time to 1-min intervals
  data$datetime <- parse_date_time(data$datetime, orders = c("mdy_HM", "ymd_HM"))
  
  # Check for finite datetime values and stop if any are found
  if(any(!is.finite(data$datetime))) {
    stop(paste("Non-finite datetime values found in file", file))
  }
  
  # Create a complete series with 1-min intervals
  full_dates <- seq(min(data$datetime), max(data$datetime), by = "1 min")
  data_full <- full_dates %>% as_tibble() %>% rename(datetime = value)
  data <- full_join(data_full, data, by = "datetime")
  data$P_mm[is.na(data$P_mm)] <- 0
  
  # Group by 10-minute intervals and sum the values
  data_10min <- data %>%
    group_by(datetime_10min = floor_date(datetime, "10 minutes")) %>%
    summarise(P_mm = sum(P_mm, na.rm = TRUE))
  
  # Add coordinates
  file_name <- basename(file)
  file_coords <- filter(coords, filename == file_name)
  data_10min$latitude <- file_coords$latitude
  data_10min$longitude <- file_coords$longitude
  
  # Create new column combining longitude and latitude
  data_10min <- data_10min %>% 
    mutate(location = paste(longitude, latitude, sep = "_")) %>%
    dplyr::select(-c(latitude, longitude))
  
  
  return(data_10min)
}

# Process all files and bind them together
all_data <- map_dfr(file_list, process_file)

# Print the first few rows of the new data to check
print(head(all_data))

# Pivot data from long to wide format and replace NA with 0
wide_data <- all_data %>%
  pivot_wider(names_from = location, values_from = P_mm, values_fill = 0)

write.csv(wide_data, "new_bucket_10min.csv")


##########################

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

# Load the required library
library(tidyverse)
library(data.table)

# Read in your second data frame
all_data <- read_csv("new_MRMS_10min.csv")
all_data_2 <- read_csv("new_bucket_10min.csv")



# Convert the dataframes to a long format
df1_long <- pivot_longer(all_data, cols = -datetime, names_to = "location", values_to = "P_mm")
df2_long <- pivot_longer(all_data_2, cols = -datetime, names_to = "location", values_to = "P_mm")

# Merge the long format dataframes
merged_df_long <- bind_rows(df1_long, df2_long)

# Convert back to a wide format
# Convert back to a wide format, taking the sum of any duplicate values
# Convert back to a wide format, taking the sum of any duplicate values
merged_df <- pivot_wider(merged_df_long, names_from = location, values_from = "P_mm", values_fn = list(P_mm = ~sum(.x, na.rm = TRUE)))

# Write the merged dataframe to a new CSV file
write.csv(merged_df, "merged_10min.csv")
