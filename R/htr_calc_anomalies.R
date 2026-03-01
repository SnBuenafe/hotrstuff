#' Calculate anomalies relative to the baseline mean
#'
#' This function calculates climate anomalies by subtracting baseline mean values
#' from projection data using CDO (Climate Data Operators). It processes multiple
#' climate model files in parallel, matching variables, frequencies, and models
#' between the projection data and baseline means.
#'
#' @details
#' The function uses the CDO `sub` operator to subtract baseline means from
#' projection files. It automatically matches files based on variable, frequency,
#' and model metadata extracted from CMIP6-formatted filenames. The process runs
#' in parallel using multiple CPU cores for efficient processing of large datasets.
#'
#' The workflow involves:
#' 1. Extracting metadata from baseline mean files
#' 2. Finding corresponding projection files for each variable-frequency-model combination
#' 3. Subtracting the appropriate baseline mean from each projection file using CDO
#' 4. Saving results with "_anomalies_" in the filename
#'
#' @inheritParams htr_slice_period
#' @param mndir Character string. The directory where the baseline mean files are
#'   stored. Files should follow CMIP6 naming conventions with variable, frequency,
#'   and model information in the filename.
#' @param overwrite Logical. If `FALSE` (default), skips files that already exist
#'   in the output directory. If `TRUE`, regenerates files even if they exist.
#'
#' @return
#' No return value. The function creates anomaly files in the specified output
#' directory with "_anomalies_" replacing "_merged_" in the original filenames.
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Input files must follow CMIP6 naming conventions for proper metadata extraction
#' - Baseline mean files and projection files must have matching variable, frequency, and model names
#' - Uses parallel processing with (number of CPU cores - 2) workers
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO sub operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=297
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_calc_anomalies(
#'   indir = file.path(base_dir, "data", "tos", "raw"),
#'   indir = file.path(base_dir, "data", "tos", "mean"),
#'   outdir = file.path(base_dir, "data", "tos", "anomalies")
#' )
#' }
htr_calc_anomalies <- function(indir, # input directory of the projections
                               mndir, # directory of baseline mean
                               outdir, # where anomalies will be saved
                               overwrite = FALSE # if TRUE, overwrite existing files
) {
  w <- parallel::detectCores() - 2

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Check for files in input directories
  all_files <- htr_list_files(indir)
  if (is.null(all_files)) return(invisible(NULL))

  all_mn <- htr_list_files(mndir)
  if (is.null(all_mn)) return(invisible(NULL))

  # get metadata from the files in the baseline directory
  x <- htr_get_meta(mndir,
    string = c("Variable", "Frequency", "Model")
  )

  ##############

  do_anom <- function(v, fr, m) {
    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", v, "_", ")(?=.*", fr, "_", ")(?=.*", m, "_", ")")) # For each combination of variable-frequency-model that we have a baseline mean for, find merged files for all time periods

    ##############
    subtract_mean <- function(f, overwrite) {
      . <- NULL # Stop devtools::check() complaints about NSE

      bits <- basename(f) %>%
        htr_get_CMIP6_bits()

      mn <- dir(mndir, pattern = paste0(bits$Variable, "_", bits$Frequency, "_", bits$Model)) %>%
        paste0(mndir, "/", .)

      anom_out <- stringr::str_replace_all(f, dirname(f), outdir) %>%
        stringr::str_replace("_merged_", "_anomalies_")

      cdo_code <- paste0("cdo sub ", f, " ", mn, " ", anom_out)
      htr_run_cdo(cdo_code, anom_out, overwrite)
    }
    ##############

    purrr::walk(files, subtract_mean, overwrite)
  }

  ##############

  future::plan(future::multisession, workers = w)
  furrr::future_pwalk(x, do_anom)
  future::plan(future::sequential)
}
