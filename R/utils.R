#' Convert a raster mask to netCDF4 format for CDO compatibility
#'
#' This internal function converts a terra raster object to a netCDF4 file that
#' is compatible with CDO (Climate Data Operators). It's primarily used for
#' creating grid template files for regridding operations.
#'
#' @details
#' The function creates a properly formatted netCDF4 file with:
#' - Longitude and latitude dimensions with appropriate attributes
#' - Time dimension (set to 1850-01-01 for compatibility)
#' - 365-day calendar attribute for consistency with climate data
#' - Proper axis attributes (X, Y, T) for CDO recognition
#' - netCDF4 "classic model" format for CDO compatibility
#'
#' The process involves creating a temporary netCDF3 file, converting it to
#' netCDF4 format using `nccopy`, and then using CDO to invert latitude
#' ordering for proper orientation.
#'
#' @author David Schoeman
#'
#' @param x A terra SpatRaster object to be converted to netCDF
#' @param pth Character string. Directory path where the netCDF file will be saved
#' @param ncName Character string. Name of the output netCDF file
#' @param dname Character string. Variable name in the netCDF file
#' @param dlname Character string. Long name for the variable
#'
#' @return
#' Character string. Full path to the created netCDF file.
#'
#' @note
#' - Requires `nccopy` utility (part of netCDF tools) to be available
#' - Requires CDO for latitude inversion (`cdo -invertlat`)
#' - Creates temporary files that are automatically cleaned up
#' - Uses 365-day calendar for consistency with climate model data
#'
#' @references
#' Based on: http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html#create-and-write-a-netcdf-file
#'
#' @noRd
htr_mask2netCDF4 <- function(x,
                             pth = paste0(getwd(), "/", "Data"),
                             ncName = "mask.nc",
                             dname = "tos",
                             dlname = "tos") {

  nc_name <- file.path(pth, ncName) # Input netCDF

  # Ensure the directory exists
  htr_make_folder(pth)

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
  time <- lubridate::time_length(lubridate::interval(lubridate::ymd_hms("1850-01-01-00:00:00"), "1850-01-01"), unit = "day")
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

  return(nc_name)
}



#' Create directory if it doesn't exist
#'
#' This utility function creates a directory (and any necessary parent directories)
#' if it doesn't already exist. It's used throughout the hotrstuff package to
#' ensure output directories are available before processing.
#'
#' @param folder Character string. Path to the directory to be created. Can be
#'   relative or absolute path. Parent directories will be created recursively
#'   if they don't exist.
#'
#' @return
#' No return value. The function creates the directory structure as needed.
#'
#' @note
#' - Uses `dir.create()` with `recursive = TRUE` to create parent directories
#' - Checks if directory already exists before attempting creation
#' - No error is thrown if directory already exists
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_make_folder("~/Data/output")
#' htr_make_folder("./results/processed/regridded")
#' }
htr_make_folder <- function(folder) {
  if (!isTRUE(file.info(folder)$isdir)) dir.create(folder, recursive = TRUE)
}


#' Create a blank raster template for regridding operations
#'
#' This internal function creates a global regular latitude-longitude raster
#' template at the specified resolution and converts it to netCDF4 format for
#' use as a regridding target in CDO operations.
#'
#' @details
#' The function creates a global raster covering -180 to 180 degrees longitude
#' and -90 to 90 degrees latitude at the specified resolution. All cells are
#' set to value 1, and the raster is converted to netCDF4 format using
#' `htr_mask2netCDF4()` for compatibility with CDO regridding operations.
#'
#' @author David Schoeman and Tin Buenafe
#'
#' @param out_dir Character string. Directory where the template file will be created
#' @param cell_res Numeric. Spatial resolution in degrees (e.g., 0.25 for quarter-degree)
#'
#' @return
#' Character string. Full path to the created netCDF template file.
#'
#' @note
#' - Creates a file named "base_rast.nc" in the output directory
#' - Template file should be deleted after regridding operations
#' - Uses terra::rast() to create the initial raster template
#'
#' @noRd
htr_make_blankRaster <- function(out_dir, cell_res # resolution of the cell
) {

  r <- terra::rast(resolution = cell_res)
  r[] <- 1

  base_rast <- htr_mask2netCDF4(r,
                                pth = out_dir,
                                ncName = "base_rast.nc",
                                dname = "dummy",
                                dlname = "dummy"
  ) # changes the base_rast to netcdf4

  return(base_rast)
}




#' Extract data from a specific year range using CDO
#'
#' This internal function extracts data from a specified year range using CDO's
#' `selyear` operator. It's used by [`htr_slice_period()`] to perform the actual
#' time slicing operations and handles filename generation for the output.
#'
#' @details
#' The function checks if the input file's time range extends beyond the requested
#' years and uses CDO to extract only the specified period. It automatically
#' generates appropriate output filenames with the new time range.
#'
#' The CDO command used is:
#' `cdo selyear,year_start/year_end input_file output_file`
#'
#' @author David Schoeman and Tin Buenafe
#'
#' @param nc_file Character string. Name of the input NetCDF file
#' @param yr1 Numeric. Starting year for extraction
#' @param yr2 Numeric. Ending year for extraction
#' @param infold Character string. Input directory path
#' @param outfold Character string. Output directory path
#' @param overwrite Logical. Whether to overwrite existing files
#'
#' @return
#' No return value. Creates time-sliced file in the output directory.
#'
#' @note
#' - Only processes files if their time range extends beyond the requested period
#' - Automatically generates output filenames with new time ranges
#' - Uses CMIP6 filename parsing for metadata extraction
#'
#' @noRd
htr_get_Years <- function(nc_file, yr1, yr2, infold, outfold, overwrite) {
  . <- NULL # Stop devtools::check() complaints about NSE

  bits <- htr_get_CMIP6_bits(nc_file)

  y1 <- lubridate::year(bits$Year_start)
  y2 <- lubridate::year(bits$Year_end)

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






#' Extract metadata combinations from CMIP6 filenames
#'
#' This internal function extracts unique combinations of specified metadata
#' elements (variable, frequency, scenario, model, variant) from CMIP6-formatted
#' filenames in a directory. It's used to organize parallel processing tasks
#' by grouping files with common characteristics.
#'
#' @details
#' The function processes all files in a directory, extracts CMIP6 metadata
#' using `htr_get_CMIP6_bits()`, and returns unique combinations of the
#' requested metadata elements. This is essential for organizing batch
#' processing operations where files need to be grouped by their characteristics.
#'
#' @author David Schoeman and Tin Buenafe
#'
#' @param x Character string. Directory path containing CMIP6 files
#' @param string Character vector. Metadata elements to extract (e.g.,
#'   c("Variable", "Frequency", "Model", "Scenario", "Variant"))
#'
#' @return
#' List of character vectors containing unique combinations of the requested
#' metadata elements, suitable for use with parallel processing functions.
#'
#' @note
#' - Requires files to follow CMIP6 naming conventions
#' - Returns distinct combinations only (no duplicates)
#' - Output format is compatible with purrr::pwalk() for parallel processing
#'
#' @noRd
htr_get_meta <- function(x,
                         string # refers to the aspects extracted per climate model
) {
  y <- dir(x) %>%
    purrr::map(htr_get_CMIP6_bits) %>%
    purrr::map(`[`, string) %>%
    purrr::map(dplyr::bind_cols) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    as.list() %>%
    unname()

  return(y)
}




#' Parse CMIP6 filename components and metadata
#'
#' This internal function parses CMIP6-formatted filenames to extract metadata
#' components including variable, frequency, model, scenario, variant, grid,
#' and time range information. It's essential for organizing and processing
#' climate model data based on their characteristics.
#'
#' @details
#' CMIP6 files follow a standardized naming convention:
#' `variable_frequency_model_scenario_variant_grid_timerange.nc`
#'
#' The function:
#' 1. Splits the filename by underscores to extract components
#' 2. Parses the time range component (7th element) to extract start/end dates
#' 3. Handles different frequency formats (monthly, yearly) by adjusting date formats
#' 4. Converts date strings to Date objects for proper temporal handling
#'
#' Special handling for different frequencies:
#' - Monthly data (`_.mon_`): Adds day 01 and 31 to start/end dates
#' - Yearly data (`_.year_`): Adds full date range (0101-1231) to years
#'
#' @author David Schoeman and Tin Buenafe
#'
#' @param file_name Character string. CMIP6-formatted filename to parse
#'
#' @return
#' Named list containing:
#' - `Variable`: Climate variable name (e.g., "tos", "pr", "tas")
#' - `Frequency`: Temporal frequency (e.g., "Omon", "day", "Amon")
#' - `Model`: Climate model name (e.g., "ACCESS-ESM1-5", "CanESM5")
#' - `Scenario`: Experiment/scenario (e.g., "historical", "ssp126")
#' - `Variant`: Variant label (e.g., "r1i1p1f1")
#' - `Grid`: Grid label (e.g., "gn", "gr")
#' - `Year_start`: Start date as Date object
#' - `Year_end`: End date as Date object
#'
#' @note
#' - Assumes standard CMIP6 filename format with 7 underscore-separated components
#' - Handles different temporal frequency formats automatically
#' - Returns Date objects for proper temporal operations
#' - Used throughout the package for file organization and metadata extraction
#'
#' @references
#' CMIP6 Data Reference Syntax: https://pcmdi.llnl.gov/CMIP6/Guide/dataUsers.html
#'
#' @noRd
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
}
