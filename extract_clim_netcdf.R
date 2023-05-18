# Much of this code was adapted from the tutorial by Maria Brauner found here: https://towardsdatascience.com/how-to-crack-open-netcdf-files-in-r-and-extract-data-as-time-series-24107b70dcd

# Load packages -------------------------------------------------

library(ncdf4) 
library(tidyverse) 

# Load data -----------------------------------------------------
setwd("~/R/Bennett")
our_nc_data <- nc_open("ERA5L_combined.nc") #this is your merged netcdf file

# Start exploring and extracting --------------------------------

varnames <- attributes(our_nc_data$var) 

## Get Lat and Lon

lat <- ncvar_get(our_nc_data,"latitude") #get Latitudes into its own vector/object
nlat <- dim(lat)
lon <- ncvar_get(our_nc_data, "longitude") #get Longitudes into its own vector/object
nlon <- dim(lon)

lat
lon
print(c(nlon, nlat)) #will tell you grid dimensions

# Now we get the time variable
time <- ncvar_get(our_nc_data,"time") #get observation_times 
head(time)
tunits <- ncatt_get(our_nc_data, "time","units") #units, days since string (make readable dates & time later)
nt <- dim(time)


# Data Frame Creation ----------------------------------------------------

# Fix to readable time
#our print(tunits) remind us that our time origin is hours since 1900-01-01 00:00:00
time_obs <- as.POSIXct(time*3600, origin = "1900-01-01") #, tz="MST"
dim(time_obs) 
range(time_obs) #Range: should be 12/30/20-11/18/22


#define vars
var_list <- c("t2m", "d2m", "u10", "v10", "ssrd")  # You can add more variables to this list

# Create an empty data frame
era5L_obs <- data.frame()

# Create 2D matrix of long, lat and time
lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))# this might take several seconds

# Loop over each variable in the list
for (var in var_list) {
  era5L_array <- ncvar_get(our_nc_data, var) #get the variable in matrix slices
  
  fillvalue <- ncatt_get(our_nc_data, var, "_FillValue")
  
  # replace nc Fillvalues with R NAs
  era5L_array[era5L_array==fillvalue$value] <- NA
  
  #reshape whole array
  era5L_vec_long <- as.vector(era5L_array)
  
  # Append the vector to the lonlattime matrix
  lonlattime <- cbind(lonlattime, era5L_vec_long)
}

# Create the final dataframe
era5L_obs <- data.frame(lonlattime)

# Change column names
colnames(era5L_obs) <- c("Long","Lat","Date", var_list)



# Clean it up, Conversions, Calculations --------------------------------------------

# correct the data types:
era5L_obs$Date <- as_datetime(era5L_obs$Date)
for (var in var_list) {
  era5L_obs[[var]] <- as.double(era5L_obs[[var]])
}

# Conversions
era5L_obs$t2m <- era5L_obs$t2m-273.15 #TEMP ONLY CONVERSION, K to C
era5L_obs$d2m <- era5L_obs$d2m-273.15 #TEMP ONLY CONVERSION, K to C
era5L_obs <- era5L_obs %>% #WIND ONLY CONVERSION, xy vectors to m/s
  mutate(wind = sqrt(u10^2 + v10^2))%>% 
  select(-c(u10, v10))
era5L_obs$ssrd <- era5L_obs$ssrd/1e6

#RH calculation
calculate_relative_humidity <- function(era5L_obs) {
  T = era5L_obs$t2m 
  TD = era5L_obs$d2m 
  RH = 100 * (exp((17.625*TD)/(243.04+TD)) / exp((17.625*T)/(243.04+T))) #August-Roche-Magnus approximation
  return(RH)
}
era5L_obs$RH = calculate_relative_humidity(era5L_obs)
era5L_obs = subset(era5L_obs, select = -c(d2m))


# Final Reshaping and cleanup --------------------------------------
# Reshape the data frame so that each grid cell is a separate column

var_list2 <- c("t2m", "ssrd", "wind", "RH")  

era5L_obs <- era5L_obs %>%
  unite("cell", Long, Lat, remove = TRUE)

# Initialize a list to store each dataframe
list_df <- list()

# Loop over each variable and pivot it into wider format
for (var in var_list2) {
  temp_df <- era5L_obs %>%
    select(Date, cell, var) %>%
    pivot_wider(names_from = cell, values_from = var)
  
  # Store the dataframe in the list
  list_df[[var]] <- temp_df
}


# Loop over var_list2, convert list columns to character, and replace NA values
for (var in var_list2) {
  temp_df <- list_df[[var]]
  
  # Convert list columns to character and replace NA values with -9999
  temp_df <- temp_df %>%
    mutate(across(starts_with("-"), ~replace_na(., "-9999")))%>%
    mutate(across(starts_with("-"), ~sapply(., toString))) 
    
  
  # Assign the dataframe to a variable in global R environment
  assign(var, temp_df, envir = .GlobalEnv)
}


# Write to csv----------------------------------------------
for (var in var_list2) {
  write.csv(get(var), paste0("ERA5L_", var, ".csv"), row.names = T)
}

  