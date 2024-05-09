# Function to convert a raster mask to a netCDF
# Based on http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html#create-and-write-a-netcdf-file
# Written by David Schoeman
# Last modified: October 2023 

pacman::p_load(tidyverse, lubridate, terra, ncdf4)

mask2netCDF4 <- function(x, pth = paste0(getwd(), "/", "Data"), 
                         ncName = "mask.nc", 
                         dname = "tos", 
                         dlname = "tos")	{
  nc_name <- paste0(pth, "/", ncName) # Input netCDF
  # Temporary files
  nc1 <- nc_name %>% 
    str_replace(".nc", "_tmp1.nc")
  nc2 <- nc_name %>% 
    str_replace(".nc", "_tmp2.nc")
  r1out <- x[] # Write mask as a matrix
  # Set up the temporal and spatial dimensions	
  lon <- terra::xFromCol(x, 1:ncol(x)) # Lons - from raster
  nlon <- length(lon)
  lat <- yFromRow(x, 1:nrow(x)) # Lats from raster
  nlat <- length(lat)
  time <- time_length(interval(ymd_hms("1850-01-01-00:00:00"), "1850-01-01"), unit = "day")
  nt <- length(time)
  tunits <- "days since 1850-01-011 00:00:00.0 -0:00"
  # Use this to build a multi-layer array	
  tmp_array <- array(r1out, dim=c(nlon, nlat, nt)) # Write as an array
  # Set neCDF variables and dimensions
  londim <- ncdim_def("lon","degrees_east", as.double(lon), calendar = "365_day", longname = "longitude") 
  latdim <- ncdim_def("lat","degrees_north", as.double(lat), calendar = "365_day", longname = "latitude") 
  timedim <- ncdim_def("time", tunits, as.double(time), calendar = "365_day", longname = "time")
  fillvalue <- missvalue <- 1.00000002004088e+20 # Na values
  tmp_def <- ncvar_def(dname,"deg_C", list(londim, latdim, timedim), missvalue, dlname, prec = "double")
  # Create netCDF file and assign arrays
  ncout <- nc_create(nc1, list(tmp_def)) # Don't force it to be netCDF4, or CDO will fail
  ncvar_put(ncout, tmp_def, tmp_array)
  # Put additional attributes into dimension and data variables
  ncatt_put(ncout, "lon", "axis", "X")
  ncatt_put(ncout, "lat", "axis", "Y")
  ncatt_put(ncout, "time", "axis", "T")
  system(paste0("nccopy -k 4 ", nc1, " ", nc2)) # Convert to netCDF4 "classic model" mode for CDO to be able to read it
  system(paste0("cdo -invertlat ", nc2, " ", nc_name)) # Convert to netCDF4 "classic model" mode for CDO to be able to read it
  system(paste0("rm ", nc1, " ", nc2))
}	