# Load required packages
library(tidyverse)
library(lubridate)
library(readr)

# Specify the directory where your csv files are located
setwd("~/R/Bennett/MRMS/combine")
csv_directory <- setwd("~/R/Bennett/MRMS/combine")

# List all csv files in the directory
csv_files <- list.files(path = csv_directory, pattern = "*.csv")

# Create a function to standardize datetime format
standardize_datetime <- function(dt) {
  if (!is.na(dt) && str_detect(dt, "\\d{4}-\\d{2}-\\d{2}")) {  # If in 'yyyy-mm-dd' format
    dt <- format(as.POSIXct(dt, format = "%Y-%m-%d %H:%M:%S"), "%m/%d/%Y %H:%M")
  }
  return(dt)
}

# Create a function to read and standardize CSV
read_standardize_csv <- function(file) {
  # Read the csv file, ensuring datetime is read as character
  data <- read_csv(file, col_types = cols(datetime = col_character()))
  
  # Check the column names
  if (!identical(colnames(data), colnames(read_csv(csv_files[1], col_types = cols(datetime = col_character()))))) {
    stop("Column names in ", file, " do not match the column names in the first file.")
  }
  
  # Standardize datetime format
  data$datetime <- sapply(data$datetime, standardize_datetime)
  
  # Return the data
  return(data)
}

# Use lapply to read and standardize all csv files, each into its own dataframe in a list
list_of_dataframes <- lapply(csv_files, read_standardize_csv)

# Combine all dataframes into one
combined_data <- bind_rows(list_of_dataframes)

# Convert 'datetime' from character to datetime object using multiple possible formats
combined_data$datetime <- parse_date_time(combined_data$datetime, orders = c("mdy_HM", "Ymd_HMS"))

# Remove rows with NA in the datetime column
combined_data <- combined_data %>% filter(!is.na(datetime))

# Remove duplicate rows
combined_data <- distinct(combined_data)

# Sort the data in chronological order
sorted_data <- combined_data %>% arrange(datetime)

# Convert the datetime column to a datetime object in MDT
sorted_data$datetime <- force_tz(sorted_data$datetime, tzone = "America/Denver")

# Shift the timestamps by an hour
sorted_data <- sorted_data %>% mutate(datetime = datetime - hours(1))

# Remove rows with NA in the datetime column
sorted_data <- sorted_data %>% filter(!is.na(datetime))

# Create a sequence from min to max datetime in 1-minute intervals
one_minute_sequence <- seq(from = min(sorted_data$datetime), to = max(sorted_data$datetime), by = "1 min")

# Create a new dataframe with this sequence
interpolated_data <- data.frame(datetime = one_minute_sequence)

# Get the column names of the rain intensity measurements
rain_cols <- grep("^-105", names(sorted_data), value = TRUE)

# Interpolate the rain intensity measurements and assign to the new dataframe
for (col in rain_cols) {
  # Original x and y
  x <- as.numeric(sorted_data$datetime)
  y <- sorted_data[[col]]
  
  # New x
  x_new <- as.numeric(interpolated_data$datetime)
  
  # Interpolate y at new x
  y_new <- approx(x = x, y = y, xout = x_new)$y / 60
  
  # Assign the interpolated data to the new dataframe
  interpolated_data[[col]] <- y_new
}

# Convert from inches to mm
for (col in rain_cols) {
  interpolated_data[[col]] <- interpolated_data[[col]] * 25.4
}

# Replace any NA values with -9999
for (col in rain_cols) {
  interpolated_data[[col]] <- replace_na(interpolated_data[[col]], -9999)
}

# Convert datetime to character, preserving timezone
interpolated_data$datetime <- as.character(interpolated_data$datetime)

# Write the interpolated data to a new csv file
write_csv(interpolated_data, "interpolated_data_6.csv")
