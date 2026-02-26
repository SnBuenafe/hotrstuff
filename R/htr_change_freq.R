#' Change temporal frequency of climate data
#'
#' This function changes the temporal frequency of climate data from daily to either
#' monthly or yearly averages using CDO (Climate Data Operators). It supports both
#' HPC array job processing and parallel processing for efficient computation.
#'
#' @details
#' The function uses CDO temporal aggregation operators to change frequency:
#' - For yearly frequency: Uses `cdo -yearmean` to calculate annual means
#' - For monthly frequency: Uses `cdo -monmean` to calculate monthly means
#'
#' The function can operate in different modes:
#' - **Array mode** (`hpc = "array"`): Processes a single specified file (useful for HPC job arrays)
#' - **Parallel mode** (`hpc = "parallel"` or `hpc = NA`): Processes all files in the input directory using parallel workers
#'
#' Output files are renamed to reflect the new temporal frequency, replacing "_merged_"
#' with either "_annual_" or "_monthly_" in the filename.
#'
#' @author Tin Buenafe
#'
#' @inheritParams htr_slice_period
#' @param freq Character string. The target temporal frequency. Valid options are:
#'   - `"yearly"` or `"annual"`: Calculate annual means using CDO yearmean
#'   - `"monthly"`: Calculate monthly means using CDO monmean
#'
#' @return
#' No return value. The function creates frequency-converted files in the specified
#' output directory with "_annual_" or "_monthly_" replacing "_merged_" in the
#' original filenames.
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Input files should typically be daily frequency data for meaningful aggregation
#' - For HPC environments, set `hpc = "array"` and specify the `file` parameter
#' - Uses parallel processing when `hpc = NA` or `hpc = "parallel"`
#' - Worker count is automatically determined based on available CPU cores
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO yearmean operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=191
#' CDO monmean operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=186
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_change_freq(
#'   hpc = NA,
#'   file = NA,
#'   freq = "monthly",
#'   indir = file.path(".", "data", "proc", "sliced", variable),
#'   outdir = file.path(".", "data", "proc", "monthly", variable)
#' )
#' }
htr_change_freq <- function(indir,
                            outdir,
                            freq, # possible values are "yearly" or "monthly"
                            ncores = NULL, # Use all available. Ignored on HPC
                            hpc = NULL, # if run in the HPC, possible values are "array", "parallel"
                            file = NA, # hpc = "array", the input will be the file
                            cdo_flags = "-f nc4c -z zip_1"
) {
  . <- NULL # Stop devtools::check() complaints about NSE


  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Define workers
  w <- htr_workers(ncores, hpc)

  ##############

  change_yearly <- function(f, outdir) {
    out_file <- f %>%
      basename() %>%
      stringr::str_split("_merged_") %>%
      purrr::map(~ paste0(.x[1], "_annual_", .x[2])) %>%
      paste0(outdir, "/", .)

    cdo_code <- paste0("cdo ", cdo_flags, " -yearmean", " ", f, " ", out_file)

    system(cdo_code)
  }

  ##############

  change_monthly <- function(f, outdir) {
    out_file <- f %>%
      basename() %>%
      stringr::str_split("_merged_") %>%
      purrr::map(~ paste0(.x[1], "_monthly_", .x[2])) %>%
      paste0(outdir, "/", .)

    cdo_code <- paste0("cdo ", cdo_flags, " -monmean", " ", f, " ", out_file)

    system(cdo_code)
  }

  ##############

  # TODO Why not just have workers = 1 for this, rather than if else

  if (isTRUE(hpc %in% "array")) { # For hpc == "array", use the specific files as the starting point


    #TODO Add warning if no files are detected. At the moment this just exits quietyl with no warning.
    esm <- dir(indir, pattern = file, full.names = TRUE)

    if (stringr::str_to_lower(freq) == "yearly") { # run function
      change_yearly(esm, outdir)
    } else if (stringr::str_to_lower(freq) == "monthly") {
      change_monthly(esm, outdir)
    }

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

    esms <- dir(indir, pattern = "*.nc", full.names = TRUE)

    future::plan(future::multisession, workers = w)

    if (stringr::str_to_lower(freq) == "yearly") {
      furrr::future_walk(esms, change_yearly, outdir) # JDE
    } else if (stringr::str_to_lower(freq) == "monthly") {
      furrr::future_walk(esms, change_monthly, outdir) # JDE
    }

    future::plan(future::sequential)

  }
}
