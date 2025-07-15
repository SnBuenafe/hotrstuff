#' Calculate temporal mean of specified time period
#'
#' This function calculates temporal means over a specified time period using CDO
#' (Climate Data Operators). It is primarily used to calculate baseline climatological
#' means from historical climate data, which can then be used for anomaly calculations.
#'
#' @details
#' The function uses the CDO `timmean` operator combined with `selyear` to calculate
#' temporal means over the specified year range. It processes files in parallel for
#' efficient computation of large climate datasets. The function automatically
#' generates output filenames with "_mean_" and the year range in the filename.
#'
#' The CDO command executed is:
#' `cdo -L -timmean -selyear,year_start/year_end input_file output_file`
#'
#' Where:
#' - `-L` enables netCDF4 compression
#' - `timmean` calculates the temporal mean
#' - `selyear` selects the specified year range
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @inheritParams htr_slice_period
#' @param scenario Character string. The CMIP6 scenario to process (e.g., "historical",
#'   "ssp126", "ssp245"). Use "historical" for calculating baseline climatological means.
#' @param year_start Numeric. Starting year for calculating the temporal mean (inclusive).
#' @param year_end Numeric. Ending year for calculating the temporal mean (inclusive).
#'
#' @return
#' No return value. The function creates mean files in the specified output directory
#' with "_mean_YYYYMMDD-YYYYMMDD.nc" replacing "_merged_" in the original filenames,
#' where the dates represent the start and end of the averaging period.
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Input files must be merged time series files (typically created by [`htr_merge_files()`])
#' - Uses parallel processing with (number of CPU cores - 2) workers
#' - The `-L` flag enables netCDF4 compression for smaller output files
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO timmean operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=180
#' CDO selyear operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=124
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_calc_mean(
#'   indir = file.path(base_dir, "data", "tos", "raw"),
#'   outdir = file.path(base_dir, "data", "tos", "mean"),
#'   scenario = "historical",
#'   year_start = 1950,
#'   year_end = 2014
#' )
#' }
htr_calc_mean <- function(indir, # where inputs are
                          outdir, # where outputs will be saved
                          scenario, # historical or ssp (use historical for calculating baseline means)
                          year_start, # start year for calculating mean of time period
                          year_end # end year for calculating mean of time period
) {
  . <- NULL # Stop devtools::check() complaints about NSE

  w <- parallelly::availableCores(method = "system", omit = 2)

  ##############

  get_mean <- function(f) {
    out_file <- f %>%
      basename() %>%
      stringr::str_split("_merged_") %>%
      purrr::map(~ paste0(.x[1], "_mean_", year_start, "0101-", year_end, "1231.nc")) %>%
      paste0(outdir, "/", .)
    cdo_code <- paste0("cdo -L -timmean -selyear,", year_start, "/", year_end, " ", f, " ", out_file)
    system(cdo_code)
  }

  ##############

  esms <- dir(indir, pattern = scenario, full.names = TRUE)
  future::plan(future::multisession, workers = w)
  furrr::future_walk(esms, get_mean)
  future::plan(future::sequential)
}
