#' Function to convert a raster mask to a netCDF
#'
#'
#' Based on http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html#create-and-write-a-netcdf-file
#' pacman::p_load(tidyverse, lubridate, terra, ncdf4)
#'
#' @author David Schoeman
#'
#' @param x
#' @param pth
#' @param ncName
#' @param dname
#' @param dlname
#'
#' @return
#' @export
#'
#' @examples
htr_mask2netCDF4 <- function(x, pth = paste0(getwd(), "/", "Data"),
                         ncName = "mask.nc",
                         dname = "tos",
                         dlname = "tos") {
  nc_name <- paste0(pth, "/", ncName) # Input netCDF

  # Temporary files
  nc1 <- nc_name %>%
    stringr::str_replace(".nc", "_tmp1.nc")
  nc2 <- nc_name %>%
    stringr::str_replace(".nc", "_tmp2.nc")
  r1out <- x[] # Write mask as a matrix

  # Set up the temporal and spatial dimensions
  lon <- terra::xFromCol(x, 1:ncol(x)) # Lons - from raster
  nlon <- length(lon)
  lat <- terra::yFromRow(x, 1:nrow(x)) # Lats from raster
  nlat <- length(lat)
  time <- time_length(interval(lubridate::ymd_hms("1850-01-01-00:00:00"), "1850-01-01"), unit = "day")
  nt <- length(time)
  tunits <- "days since 1850-01-011 00:00:00.0 -0:00"

  # Use this to build a multi-layer array
  tmp_array <- array(r1out, dim = c(nlon, nlat, nt)) # Write as an array

  # Set neCDF variables and dimensions
  londim <- ncdf4::ncdim_def("lon", "degrees_east", as.double(lon), calendar = "365_day", longname = "longitude")
  latdim <- ncdf4::ncdim_def("lat", "degrees_north", as.double(lat), calendar = "365_day", longname = "latitude")
  timedim <- ncdf4::ncdim_def("time", tunits, as.double(time), calendar = "365_day", longname = "time")
  fillvalue <- missvalue <- 1.00000002004088e+20 # Na values
  tmp_def <- ncdf4::ncvar_def(dname, "deg_C", list(londim, latdim, timedim), missvalue, dlname, prec = "double")

  # Create netCDF file and assign arrays
  ncout <- ncdf4::nc_create(nc1, list(tmp_def)) # Don't force it to be netCDF4, or CDO will fail
  ncdf4::ncvar_put(ncout, tmp_def, tmp_array)

  # Put additional attributes into dimension and data variables
  ncdf4::ncatt_put(ncout, "lon", "axis", "X")
  ncdf4::ncatt_put(ncout, "lat", "axis", "Y")
  ncdf4::ncatt_put(ncout, "time", "axis", "T")
  system(paste0("nccopy -k 4 ", nc1, " ", nc2)) # Convert to netCDF4 "classic model" mode for CDO to be able to read it
  system(paste0("cdo -invertlat ", nc2, " ", nc_name)) # Convert to netCDF4 "classic model" mode for CDO to be able to read it
  system(paste0("rm ", nc1, " ", nc2))
}



#' Make a folder
#'
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
htr_make_folder <- function(folder) {
  if (!isTRUE(file.info(folder)$isdir)) dir.create(folder, recursive = TRUE)
}


#' Create a blank raster
#'
#'
#' @author David Schoeman and Tin Buenafe
#'
#' @param blankrast_dir
#' @param cell_res
#'
#' @return
#' @export
#'
#' @examples
htr_make_blankRaster <- function(blankrast_dir, # directory to save the blank raster
                             cell_res # resolution of the cell
) {
  base_rast <- paste0(blankrast_dir, "/base_rast.nc")
  r <- rast(resolution = cell_res)
  r[] <- 1
  mask2netCDF4(r,
    pth = blankrast_dir,
    ncName = basename(base_rast),
    dname = "dummy",
    dlname = "dummy"
  ) # changes the base_rast to netcdf4

  return(base_rast)
}




#' Get data from range of years
#'
#'
#' pacman::p_load(lubridate, tidyverse)
#'
#' @author David Schoeman and Tin Buenafe
#'
#' @param nc_file
#' @param yr1
#' @param yr2
#' @param infold
#' @param outfold
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
htr_get_Years <- function(nc_file, yr1, yr2, infold, outfold, overwrite) {
  bits <- get_CMIP6_bits(nc_file)
  y1 <- year(bits$Year_start)
  y2 <- year(bits$Year_end)
  if ((y1 < yr1 | y2 > yr2) || isFALSE(overwrite)) {
    new_name <- nc_file %>%
      stringr::str_split(paste0("_", as.character(y1))) %>%
      purrr::map(1) %>%
      unlist() %>%
      paste0(., "_", yr1, "0101-", yr2, "1231.nc")

    system(paste0("cdo selyear,", yr1, "/", yr2, " ", infold, "/", nc_file, " ", outfold, "/", new_name))
    # file.remove(paste0(infold, "/", nc_file))
  } else {
    cat("Nothing to do!")
    cat("\n")
  }
}






#' Get combinations of variables, frequency, experiments/scenarios, models, and variants from the netCDF files
#'
#' pacman::p_load(purrr, tidyverse)
#'
#' @author David Schoeman and Tin Buenafe
#'
#' @param x
#' @param string
#'
#' @return
#' @export
#'
#' @examples
htr_get_meta <- function(x,
                     string # refers to the aspects extracted per climate model
) {
  y <- dir(x) %>%
    purrr::map(get_CMIP6_bits) %>%
    purrr::map(`[`, string) %>%
    purrr::map(dplyr::bind_cols) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    as.list() %>%
    unname()

  return(y)
}




#' Extract CMIP6 bits from the name of the file
#'
#' pacman::p_load(tidyverse)
#'
#' @author David Schoeman and Tin Buenafe
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
htr_get_CMIP6_bits <- function(file_name) {
  bits <- stringr::str_split(basename(file_name), "_") %>%
    unlist()
  date_start_stop <- bits[7] %>%
    stringr::str_split("[.]") %>%
    purrr::map(1) %>%
    unlist() %>%
    stringr::str_split("-") %>%
    unlist()
  if (stringr::str_detect(file_name, "_.mon_")) {
    date_start_stop <- paste0(date_start_stop, c("01", "31"))
  } # Fix dates for monthly data
  if (stringr::str_detect(file_name, "_.year_")) {
    date_start_stop <- paste0(date_start_stop, c("0101", "1231"))
  } # Fix dates for annual data
  date_start_stop <- as.Date(date_start_stop, format = "%Y%m%d")
  output <- list(
    Variable = bits[1],
    Frequency = bits[2],
    Model = bits[3],
    Scenario = bits[4],
    Variant = bits[5],
    Grid = bits[6],
    Year_start = date_start_stop[1],
    Year_end = date_start_stop[2]
  )
  return(output)
  # e.g., map_df(dir(folder), get_CMIP6_bits)
}
