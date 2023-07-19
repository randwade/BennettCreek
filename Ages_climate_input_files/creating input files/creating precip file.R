# Load required libraries
library(sf)
library(dplyr)
library(lubridate)
library(raster)
library(data.table)
library(microclima)
# Read CSV file without headers
setwd("~/R/Bennett/MRMS/combine")
df <- read.csv('merged_5min.csv', header = FALSE, stringsAsFactors = FALSE)

#Name output csv
csvname <- paste0("newtesty_5min",".csv")

# Get the first row, which is our header
header <- df[1,]
df <- df[-1,]

# Exclude the datetime column
header_coords <- header[-1]

# Separate longitude and latitude from header names
header_coords <- as.character(header_coords)
coords <- strsplit(header_coords, "_")

# Create a data frame from the list
coords_df <- data.frame(do.call("rbind", coords))
colnames(coords_df) <- c("longitude", "latitude")

# Convert the data frame to a spatial points data frame
coords_sp <- st_as_sf(coords_df, coords = c("longitude", "latitude"), crs = 4326)


# Load DEM data
dem <- get_dem(lat = 40.65293562851665,
                      long = -105.55619803975898,
                      resolution = 30,
                      xdims = 1200,
                      ydims = 800)
plot(dem)
plot(coords_sp, add = TRUE)


###get elevations-------
# Extract the elevation values from the DEM for each point
elevations <- extract(dem, coords_sp)

# Convert to UTM Zone 13N
utm_coords <- st_transform(coords_sp, 32613)

# Extract UTM coordinates
utm_coords_df <- data.frame(st_coordinates(utm_coords))

# Add these elevation values back to your data frame
utm_coords_df$Elevation <- elevations
utm_coords_df$X <- round(utm_coords_df$X, 1)
utm_coords_df$Y <- round(utm_coords_df$Y, 1)
# Create new column names in UTM format
new_cols <- paste(utm_coords_df$X, utm_coords_df$Y, sep="_")

# Replace old column names with new ones in the header
header[-1] <- new_cols

###CSV formatting & output-----
# Convert datetime to "dd.mm.yyyy hh:mm:ss" format
df$V1 <- format(parse_date_time(df$V1, orders = "mdy HM"), "%d.%m.%Y %H:%M:%S")

# Add updated header to data frame
colnames(df) <- header

# Prepare sequences for MRMS_# and ID

MRMS_seq <- seq(1,75)
B_seq <- c("BrownLake","PoudreCrown","BenCulv","OldFlowers","SaltCabin","ME","MM","MW","UE","UW")
name_seq <- c(paste0("MRMS_", MRMS_seq), paste0("B_", B_seq))
id_seq <- seq_along(new_cols)

# Create data frame for naming scheme
name_df <- data.frame(
  ID = id_seq,
  Name = name_seq,
  Elevation = utm_coords_df$Elevation,
  X = utm_coords_df$X,
  Y = utm_coords_df$Y
)

# Write to CSV
#write.csv(name_df, 'naming_scheme.csv', row.names = FALSE)

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
                 "name,precip.csv", 
                 "desc,gridded+measured precipitation",
                 "",
                 "missing_val,-9999", 
                 paste0("date_start,", df$datetime[1]),  
                 paste0("date_end,", tail(df$datetime, n = 1)), 
                 "date_format,dd.MM.yyyy HH:mm:ss", 
                 "",
                 "tres,d",
                 "unit,mm", 
                 "",
                 header_date,
                 header_ID,
                 header_elevation,
                 header_x,
                 header_y)


# Write the header strings to the CSV file
writeLines(all_headers, csvname)

# Add a new column at the first position
df <- cbind(Blank = "", df)

# Append the data frame to the CSV file
write.table(df, csvname, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
