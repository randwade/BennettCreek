library('mcera5')

# Create some functions modified from mcera5 for ERA5-Land ---------------------

build_era5L_request <- function(xmin, xmax, ymin, ymax, start_time, end_time,
                                outfile_name = "era5L_out") {
  
  # input checks
  if(missing(xmin)) { stop("xmin is missing")}
  if(missing(xmax)) { stop("xmax is missing")}
  if(missing(ymin)) { stop("ymin is missing")}
  if(missing(ymax)) { stop("ymax is missing")}
  if(missing(start_time)) { stop("start_time is missing")}
  if(missing(end_time)) { stop("end_time is missing")}
  
  # round to regular grid
  xmin_r <- plyr::round_any(xmin, .25, f = floor)
  xmax_r <- plyr::round_any(xmax, .25, f = ceiling)
  ymin_r <- plyr::round_any(ymin, .25, f = floor)
  ymax_r <- plyr::round_any(ymax, .25, f = ceiling)
  # area of interest
  ar <- paste0(ymax_r,"/",xmin_r,"/",ymin_r,"/",xmax_r)
  
  # create sequence of dates, one day at a time
  ut <- seq(as.POSIXct(start_time), as.POSIXct(end_time), by = "1 day")
  
  request <- list()
  
  # loop through focal days
  for(i in 1:length(ut)) {
    
    # extract year, month, and day from datetime object
    yr <- format(ut[i], "%Y")
    mon <- format(ut[i], "%m")
    day <- format(ut[i], "%d")
    
    sub_request <- list(
      "dataset_short_name" = "reanalysis-era5-land",
      "variable" = c("2m_temperature", "2m_dewpoint_temperature", "surface_pressure",
                     "10m_u_component_of_wind", "10m_v_component_of_wind",
                     "total_precipitation", "snow_depth", "snow_density",    
                     "surface_solar_radiation_downwards", "land_sea_mask"),
      "year" = yr,
      "month" = mon,
      "day" = day,
      "time" = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00",
                 "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00",
                 "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00",
                 "21:00", "22:00", "23:00"),
      "area" = ar,
      "format" = "netcdf",
      "target" = paste0(outfile_name,"_",yr,"_",mon,"_",day,".nc"))
    
    request[[i]] <- sub_request
  }
  
  return(request)
}

request_era5L <- function(request, uid, out_path, overwrite = FALSE,
                          combine = FALSE) {
  
  if (length(request) == 1 & combine) {
    cat("Your request will all be queried at once and does not need to be combined.\n")
  }
  for(req in 1:length(request)) {
    
    # Check whether file already exists for requested out_path
    if (file.exists(paste0(out_path, "/", request[[req]]$target)) & !overwrite) {
      if (length(request) > 1) {
        stop("Filename already exists within requested out_path in request ", req, " of request series. Use overwrite = TRUE if you wish to overwrite this file.")
      } else {
        stop("Filename already exists within requested out_path. Use overwrite = TRUE if you wish to overwrite this file.")
      }
    }
    
    ecmwfr::wf_request(user = as.character(uid),
                       request = request[[req]],
                       transfer = TRUE,
                       path = out_path,
                       verbose = TRUE,
                       time_out = 18000)
    
    if (file.exists(paste0(out_path, "/", request[[req]]$target))) {
      if (length(request) > 1) {
        cat("ERA5L netCDF file", req, "successfully downloaded.\n")
      } else {
        cat("ERA5L netCDF file successfully downloaded.\n")
      }
    }
  }
  if (length(request) > 1 & combine) {
    cat("Now combining netCDF files...\n")
    # Get list of filenames
    fnames <- lapply(request, function(x) {x$target})
    combine_netcdf(filenames = fnames, combined_name = "combined.nc")
    cat("Finished.\n")
  }
}


# Build request ----------------------------------------------------
# Store your UID and API key credentials as R objects


uid <- 

cds_api_key <- 


# Use `ecmwfr` package to register your machine with your credentials


ecmwfr::wf_set_key(user = uid,
                   
                   key = cds_api_key,
                   
                   service = "cds")


# Designate your desired bounding coordinates (in WGS84 / EPSG:4326)


xmn <- -105.8

xmx <- -105.3

ymn <- 40.6

ymx <- 40.7


# Designate your desired temporal extent


st_time <- lubridate::ymd("2021:01:01")

en_time <- lubridate::ymd("2022:12:31")


# Set a unique prefix for the filename (here based on spatial
# coordinates), and the file path for downloaded .nc files (here,
# the user's working directory)


file_prefix <- "give_file_a_prefix_"

file_path <- getwd()


# Build a request


req <- build_era5L_request(xmin = xmn, xmax = xmx,
                          
                          ymin = ymn, ymax = ymx,
                          
                          start_time = st_time,
                          
                          end_time = en_time,
                          
                          outfile_name = file_prefix)


# Submit your request---------------------------------------------


request_era5L(request = req, uid = uid, out_path = file_path, combine=TRUE)


