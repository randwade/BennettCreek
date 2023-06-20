# Load necessary libraries
library(rvest)
library(terra)
library(lubridate)
library(dplyr)
library(raster)
library(R.utils)
library(tidyr)

# Set the path
setwd("~/R/Bennett/MRMS")


# Create a vector of dates
dates <- seq(as.Date("2020/01/01"), as.Date("2021/12/31"), by = "day")
dates <- as.character(dates, format = "%Y/%m/%d")

# Define the extent
xmn <- -105.66
xmx <- -105.51
ymn <- 40.63
ymx <- 40.68
extent_to_crop <- ext(xmn, xmx, ymn, ymx)

# Create an output CSV file
output_csv <- "MRMS_2min_2021_2022.csv"

# Loop over each date
for (date in dates) {
  
  # Create the URL for the specific date
  url <- paste0("https://mtarchive.geol.iastate.edu/", date, "/mrms/ncep/PrecipRate/")
  
  # Use rvest to scrape the webpage
  webpage <- read_html(url)
  
  # Extract the links on the webpage
  links <- webpage %>% html_nodes("a") %>% html_attr("href")
  
  # Filter the links to get the .grib2.gz files
  file_links <- links[grepl("\\.grib2.gz$", links)]
  
  # If there is at least one .grib2.gz file
  if(length(file_links) > 0){
    
    # Loop over each file link
    for (file_link in file_links) {
      
      # Construct the full URL for the file
      file_url <- paste0(url, file_link)
      
      # Download the file to a temporary directory
      tmp_file_path <- tempfile(fileext = ".grib2.gz")
      download.file(file_url, destfile = tmp_file_path)
      
      # Unzip the file
      tmp_unzipped_path <- tempfile(fileext = ".grib2")
      gunzip(tmp_file_path, destname = tmp_unzipped_path, overwrite = TRUE)
      
      # Read the .grib2 file as a raster
      raster_data <- rast(tmp_unzipped_path)
      
      # Crop the raster to the desired extent
      cropped_raster <- crop(raster_data, extent_to_crop)
      names(cropped_raster) <- 'mmhr'
      
      # Get the timestamp from the filename
      timestamp <- substr(file_link, 18, 32)
      
      # Extract data as a data frame
      extract <- as.data.frame(cropped_raster, xy = TRUE, na.rm = FALSE)
      
      # Add timestamp, adjust time, and add day of year and hour
      extract <- extract %>%
        mutate(datetime = ymd_hms(timestamp)) %>%
        mutate(datetime = datetime - (6 * 60 * 60)) %>% #MST(daylight savings) conversion
        mutate(doy = yday(datetime)) %>%
        mutate(hour = hour(datetime))
        
      # Reshape the data frame so that each grid cell is a separate column
      extract <- extract %>%
        unite("cell", x, y, remove = TRUE) %>%
        spread(key = cell, value = "mmhr")
      
      # Write the extracted data to the CSV file
      if (file.exists(output_csv)) {
        write.table(extract, output_csv, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
      } else {
        write.table(extract, output_csv, sep = ",", col.names = TRUE, row.names = FALSE, append = FALSE)
      }
      
      # Remove temporary files
      if (file.exists(tmp_file_path)) {
        unlink(tmp_file_path, force = TRUE)
      }
      if (file.exists(tmp_unzipped_path)) {
        unlink(tmp_unzipped_path, force = TRUE)
      }



      # Optional: sleep for a few seconds between requests to avoid overloading the server
      #Sys.sleep(2)
    }
    
  } else {
    print("No .grib2.gz files found on the webpage.")
  }
}


