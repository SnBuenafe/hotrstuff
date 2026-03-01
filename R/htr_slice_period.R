#' Extract specific time periods from climate model data
#'
#' This function extracts specific time periods from merged climate model files
#' using CDO (Climate Data Operators). It is essential for focusing analysis on
#' particular time ranges of interest, such as future projection periods or
#' specific historical periods.
#'
#' @details
#' Climate model data often spans long time periods, but analysis typically focuses
#' on specific time ranges. This function uses the CDO `selyear` operator to extract
#' the specified year range from merged time series files.
#'
#' The function:
#' 1. Filters files by frequency and scenario
#' 2. For each matching file, checks if it contains data outside the target period
#' 3. If trimming is needed, uses `cdo selyear,year_start/year_end` to extract the period
#' 4. Updates filenames to reflect the new time range
#' 5. Optionally removes original files if `overwrite = TRUE`
#'
#' The CDO command used is:
#' `cdo selyear,year_start/year_end input_file output_file`
#'
#' Files are only processed if they contain data outside the specified time range,
#' making the function efficient for large datasets.
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @param indir Character string. Directory containing merged NetCDF files to be
#'   time-sliced. Files should be continuous time series created by [`htr_merge_files()`].
#' @param outdir Character string. Directory where time-sliced files will be saved.
#' @param freq Character string. CMIP6 frequency identifier to filter files
#'   (e.g., "Omon" for ocean monthly, "day" for daily, "Amon" for atmosphere monthly).
#' @param scenario Character string. CMIP6 scenario identifier to filter files
#'   (e.g., "historical", "ssp126", "ssp245", "ssp585"). Use partial strings to
#'   match multiple scenarios (e.g., "ssp" for all SSP scenarios).
#' @param year_start Numeric. Starting year for the time slice (inclusive).
#' @param year_end Numeric. Ending year for the time slice (inclusive).
#' @param overwrite Logical. If `TRUE` (default), removes original files after
#'   successful time slicing. If `FALSE`, keeps original files.
#' @param ncores Integer or NULL. Number of CPU cores to use for parallel processing.
#'   If `NULL` (default), uses all available cores minus 2. Ignored when `hpc = "array"`.
#' @param hpc Character string or NULL. Indicates High Performance Computing mode:
#'   - `NULL`: Standard processing mode (default)
#'   - `"array"`: HPC array job mode (requires `file` parameter)
#'   - `"parallel"`: HPC parallel mode
#' @param file Character string or NULL. Specific file to process when `hpc = "array"`.
#'   Not used in other modes.
#' @param cdo_flags Character string. CDO output format flags for compression.
#'   Default is `"-f nc4c -z zip_1"` for compressed NetCDF4 output.
#'
#' @return
#' No return value. The function creates time-sliced files in the specified output
#' directory with updated filenames reflecting the new time range
#' (e.g., "_merged_" becomes "_YYYYMMDD-YYYYMMDD.nc").
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Input files must follow CMIP6 naming conventions for proper metadata extraction
#' - Files are only processed if they contain data outside the specified time range
#' - Uses parallel processing when `hpc` is not set to "array"
#' - **WARNING**: Setting `overwrite = TRUE` will delete original files
#' - Progress messages show which model and scenario combinations are being processed
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO selyear operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=124
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract 21st century projection period
#' htr_slice_period(
#'   indir = file.path(base_dir, "data", "proc", "merged", "tos"),
#'   outdir = file.path(base_dir, "data", "proc", "sliced", "tos"),
#'   freq = "Omon", # ocean monthly
#'   scenario = "ssp",
#'   year_start = 2020,
#'   year_end = 2100,
#'   overwrite = FALSE
#' )
#'
#' # Extract historical baseline period
#' htr_slice_period(
#'   indir = file.path(base_dir, "data", "proc", "merged", "tos"),
#'   outdir = file.path(base_dir, "data", "proc", "sliced", "tos"),
#'   freq = "Omon",
#'   scenario = "historical",
#'   year_start = 1995,
#'   year_end = 2014,
#'   overwrite = TRUE
#' )
#' }
htr_slice_period <- function(indir, # where the merged files are
                             outdir, # where the trimmed files will be saved
                             freq, # frequency
                             scenario, # historical or ssp
                             year_start,
                             year_end,
                             overwrite = TRUE, # TRUE or FALSE
                             ncores = NULL, # Use all available. Ignored on HPC
                             hpc = NULL, # if run in the HPC, possible values are "array", "parallel"
                             file = NULL, # hpc = "array", the input will be the file
                             cdo_flags = "-f nc4c -z zip_1"
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Define workers
  w <- htr_workers(ncores, hpc)

  ##############

  trim_timeframe <- function(f) {
    s <- htr_get_CMIP6_bits(f)$Scenario
    m <- htr_get_CMIP6_bits(f)$Model

    print(paste0(m, "_", s))

    if (stringr::str_detect(s, scenario)) {
      trim_period(
        f,
        s,
        indir,
        outdir,
        year_start,
        year_end,
        overwrite,
        cdo_flags
      )
    } else {
      print(paste0("Scenario ", s, " was not chosen"))
    }
  }

  ##############

  if (isTRUE(hpc %in% "array")) { # For hpc == "array", use the specific files as the starting point

    file_n <- file[stringr::str_detect(file, freq)]

    trim_timeframe(file_n) # run function

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

    files <- htr_list_files(indir, pattern = paste0("_", freq, "_"))
    if (is.null(files)) return(invisible(NULL))
    files <- files[stringr::str_detect(files, scenario)]

    future::plan(future::multisession, workers = w)
    furrr::future_walk(files, trim_timeframe)
    future::plan(future::sequential)

  }
}






#' Trim time frame based on start and end months
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#'
#' @noRd
trim_period <- function(f, # file
                        scenario, # historical or ssp
                        indir,
                        outdir,
                        year_start,
                        year_end,
                        overwrite,
                        cdo_flags = "-f nc4c -z zip_1") {

  dt1 <- htr_get_CMIP6_bits(f)$Year_start %>%
    as.Date()

  dt2 <- htr_get_CMIP6_bits(f)$Year_end %>%
    as.Date()

  if (dt1 <= as.Date(paste0(year_start, "-01-01")) | dt2 >= as.Date(paste0(year_end, "-12-31"))) {
    htr_get_Years(f, year_start, year_end, indir, outdir, overwrite, cdo_flags) # replacing files in merged folder with trimmed files

    if (isTRUE(overwrite)) {
      terminal_code <- paste0("rm ", indir, "/", f)
      system(terminal_code)
    }
  }
}
