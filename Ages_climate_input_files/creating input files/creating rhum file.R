# Load required libraries
library(sf)
library(dplyr)
library(readr)
library(lubridate)
library(raster)
library(data.table)
library(microclima)
library(sp)
library(terra)
library(tidyverse)

#Name output csv
csvname <- paste0("ERA5L_rhum_5min",".csv")

# Set the working directory and read the CSV file
setwd("~/R/Bennett")
df <- read.csv('ERA5L_RH.csv')

# Choose the columns of interest
df <- df[,c(1,5,6,7,13,14,15)]
colnames(df)[grepl("^X\\.", colnames(df))] <- sub("^X\\.", "-", colnames(df)[grepl("^X\\.", colnames(df))])

# Rename the 'X.Date' column to 'datetime' and convert to POSIXct
df <- rename(df, datetime = Date)
df$datetime <- as.POSIXct(df$datetime, format = "%m/%d/%Y %H:%M")

# Reshape the data and interpolate
df_long <- df %>% 
  gather(key = "location", value = "t2m", -datetime) %>% 
  complete(datetime = seq(min(datetime), max(datetime), by="5 min"), 
           nesting(location), fill = list(t2m=NA))

df_long$t2m <- approx(df_long$datetime, df_long$t2m, df_long$datetime)$y

# Reshape data to wide format
df_wide <- df_long %>% spread(key = "location", value = "t2m")

# Get the column names and separate longitude and latitude
header <- colnames(df_wide)[-1]
coords <- strsplit(header, "_")

# Convert the list to a data frame and convert the strings to numeric
coords_df <- data.frame(do.call("rbind", coords))
colnames(coords_df) <- c("longitude", "latitude")
coords_df$longitude <- as.numeric(coords_df$longitude)
coords_df$latitude <- as.numeric(coords_df$latitude)

# Convert to a spatial object and transform to UTM
coords_sp <- st_as_sf(coords_df, coords = c("longitude", "latitude"), crs = 4326)
utm_coords <- st_transform(coords_sp, 32613)

# Extract UTM coordinates
utm_coords_df <- data.frame(st_coordinates(utm_coords))

# Extract the elevation values from the DEM for each point
elevations <- c(2367.6, 2357.5, 2204.7, 3327.5, 2901.6, 2650.8)

# Add these elevation values back to your data frame
utm_coords_df$Elevation <- elevations
utm_coords_df$X <- round(utm_coords_df$X, 1)
utm_coords_df$Y <- round(utm_coords_df$Y, 1)

# Create new column names in UTM format
new_cols <- paste(utm_coords_df$X, utm_coords_df$Y, sep="_")

# Replace old column names with new ones in the header
header <- new_cols

# Convert datetime to "dd.mm.yyyy hh:mm:ss" format
df_wide$datetime <- format(parse_date_time(df_wide$datetime, orders = "ymd HMS"), "%d.%m.%Y %H:%M:%S")

# Add updated header to data frame
header <- append(header, "datetime", after = 0)
colnames(df_wide) <- header

# Prepare sequences for MRMS_# and ID
ERA5L_seq <- seq(1,6)
name_seq <- paste0("ERA5L_", ERA5L_seq)
id_seq <- seq_along(new_cols)

# Create data frame for naming scheme
name_df <- data.frame(
  ID = id_seq,
  Name = name_seq,
  Elevation = utm_coords_df$Elevation,
  X = utm_coords_df$X,
  Y = utm_coords_df$Y
)

# Save the DEM as a GeoTIFF
#writeRaster(dem, filename = "dem_big.tif", format = "GTiff")

# Create the header strings
header_date <- paste("@H", "time", paste(name_seq, collapse = ","), sep = ",")
header_ID <- paste("ID", "-", paste(id_seq, collapse = ","), sep = ",")
header_elevation <- paste("elevation", "-", paste(utm_coords_df$Elevation, collapse=","), sep = ",")
header_x <- paste("x", "-", paste(utm_coords_df$X, collapse=","), sep = ",")
header_y <- paste("y", "-", paste(utm_coords_df$Y, collapse=","), sep = ",")

# Combine all header strings into one
all_headers <- c("@T,climate", 
                 "name,rhum.csv", 
                 "desc,relative humidity",
                 "",
                 "missing_val,-9999", 
                 paste0("date_start,", df_wide$datetime[1]),  
                 paste0("date_end,", tail(df_wide$datetime, n = 1)), 
                 "date_format,dd.MM.yyyy HH:mm:ss", 
                 "",
                 "unit,%", 
                 "",
                 header_date,
                 header_ID,
                 header_elevation,
                 header_x,
                 header_y)

# Write the header strings to the CSV file
writeLines(all_headers, csvname)

# Add a new column at the first position
df_wide <- cbind(Blank = "", df_wide)

# Append the data frame to the CSV file
write.table(df_wide, csvname, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
