#' Fix calendar systems by standardizing to 365-day calendar
#'
#' This function standardizes climate model data to use a consistent 365-day calendar
#' system by removing leap days (February 29th) and setting the calendar attribute.
#' This is essential for consistent temporal analysis across different climate models
#' that may use different calendar systems.
#'
#' @details
#' Climate models use various calendar systems (Gregorian, 365-day, 360-day, etc.),
#' which can cause issues when comparing or combining data from different models.
#' This function standardizes all data to a 365-day calendar using CDO operations.
#'
#' The function:
#' 1. Checks if files contain leap days by examining if the number of time steps is divisible by 365
#' 2. For daily data with leap days: Uses `cdo -setcalendar,365_day -delete,month=2,day=29` to remove February 29th
#' 3. For data without leap days: Uses `cdo setcalendar,365_day` to set the calendar attribute
#' 4. Replaces original files with the calendar-corrected versions
#'
#' The process creates temporary files during processing to avoid data corruption.
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @inheritParams htr_slice_period
#' @param indir Character string. Directory containing NetCDF files that need calendar
#'   standardization. Files should be climate model outputs with time dimensions.
#'
#' @return
#' No return value. The function modifies files in-place, replacing original files
#' with calendar-standardized versions. Progress messages are printed to the console
#' indicating which files had leap days removed.
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - **WARNING**: This function modifies files in-place. Ensure you have backups of original data
#' - Only processes daily frequency data for leap day removal (detected by "_day_" in frequency)
#' - Uses parallel processing when `hpc` is not set to "array"
#' - Creates temporary files during processing which are automatically cleaned up
#' - Prints informative messages about which files are being processed
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO setcalendar operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=142
#' CDO delete operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=60
#' CMIP6 calendar conventions: https://pcmdi.llnl.gov/CMIP6/Guide/dataUsers.html
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_fix_calendar(
#'   indir = file.path(base_dir, "data", "merged") # input directory
#' )
#' }
htr_fix_calendar <- function(indir, # input directory
                             ncores = NULL, # Use all available. Ignored on HPC
                             hpc = NULL, # if run in the HPC, possible values are "array", "parallel"
                             file = NULL, # hpc = "array", the input will be the file
                             cdo_flags = "-f nc4c -z zip_1"
) {

  . <- NULL # Stop devtools::check() complaints about NSE

  # Define workers
  w <- htr_workers(ncores, hpc)

  ##############

  fix_cal <- function(f) {
    yrs <- ncdf4::nc_open(f) %>%
      ncdf4::ncvar_get(., "time") %>%
      length(.) %% 365 # Modulo...returns zero if number of days divides by 365 without remainder
    if (yrs != 0 & stringr::str_detect(htr_get_CMIP6_bits(basename(f))$Frequency, "day_")) {
      cat(paste0(basename(f), " DOES have leap days to be removed"))
      cat("\n")
      system(paste0("cdo ", cdo_flags, " -L -setcalendar,365_day -delete,month=2,day=29 ", f, " ", dirname(f), "/tmp_", basename(f)))
      system(paste0("rm ", f))
      file.rename(paste0(dirname(f), "/tmp_", basename(f)), f)
    } else {
      cat(paste0(basename(f), " does not have leap days"))
      cat("\n")
      system(paste0("cdo ", cdo_flags, " setcalendar,365_day ", f, " ", dirname(f), "/tmp_", basename(f)))
      system(paste0("rm ", f))
      file.rename(paste0(dirname(f), "/tmp_", basename(f)), f)
    }
  }

  ##############

  if (isTRUE(hpc %in% "array")) { # For hpc == "array", use the specific files as the starting point

    file <- htr_list_files(indir, pattern = file)
    if (is.null(file)) return(invisible(NULL))

    fix_cal(file) # run function

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

    netCDFs <- htr_list_files(indir)
    if (is.null(netCDFs)) return(invisible(NULL))

    future::plan(future::multisession, workers = w)
    furrr::future_walk(netCDFs, fix_cal) # run function in parallel
    future::plan(future::sequential)

  }

}
