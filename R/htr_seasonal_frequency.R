#' Convert climate data to seasonal frequency
#'
#' This function converts monthly or daily climate data to seasonal averages by
#' selecting specific months and calculating their yearly means using CDO (Climate
#' Data Operators). This is useful for analyzing seasonal climate patterns and
#' reducing temporal resolution for specific seasonal analyses.
#'
#' @details
#' The function creates seasonal climate data through a two-step CDO process:
#' 1. **Month selection**: Uses `cdo selmon` to select only the months that define the season
#' 2. **Seasonal averaging**: Uses `cdo yearmonmean` to calculate yearly means across the selected months
#'
#' The CDO operations performed are:
#' ```
#' cdo selmon,month1,month2,month3 input_file temp_file
#' cdo yearmonmean temp_file output_file
#' ```
#'
#' This approach allows for flexible seasonal definitions (e.g., DJF for winter,
#' JJA for summer, or custom seasons like monsoon periods). The function automatically
#' updates filenames to include "_seasonal_" and the season name.
#'
#' Temporary files are used during processing and are stored in the specified
#' temporary directory.
#'
#' @inheritParams htr_slice_period
#' @param tempdir Character string. Directory for temporary files during processing.
#'   Used to store intermediate files after month selection before seasonal averaging.
#' @param months Character vector. Month numbers defining the season in two-digit
#'   format (e.g., `c("12", "01", "02")` for DJF, `c("06", "07", "08")` for JJA).
#'   Must be zero-padded (e.g., "01" not "1").
#' @param months_name Character string. Descriptive name for the season that will
#'   be added to output filenames (e.g., "DJF", "JJA", "monsoon", "dry-season").
#' @param overwrite Logical. If `FALSE` (default), skips files that already exist
#'   in the output directory. If `TRUE`, regenerates files even if they exist.
#'
#' @return
#' No return value. The function creates seasonal files in the specified output
#' directory with "_seasonal_" and the season name added to the original filenames
#' (e.g., "_merged_" becomes "_seasonal_YYYYMMDD-YYYYMMDD_seasonname.nc").
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Input data should be monthly or daily frequency for meaningful seasonal aggregation
#' - Month numbers must be zero-padded two-digit strings ("01", "02", etc.)
#' - Temporary files are created during processing but not automatically cleaned up
#' - Uses parallel processing when `hpc` is not set to "array"
#' - Ensure sufficient disk space in the temporary directory
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO selmon operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=125
#' CDO yearmonmean operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=192
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create DJF (winter) seasonal data
#' htr_seasonal_frequency(
#'   indir = here("data", "proc", "sliced", "omip", variable),
#'   tempdir = here("data", "temporary"),
#'   outdir = here("data", "proc", "seasonal", "omip", variable),
#'   months = c("12", "01", "02"), # December, January, February
#'   months_name = "DJF" # Winter season
#' )
#'
#' # Create custom monsoon season
#' htr_seasonal_frequency(
#'   indir = here("data", "proc", "sliced", "omip", variable),
#'   tempdir = here("data", "temporary"),
#'   outdir = here("data", "proc", "seasonal", "omip", variable),
#'   months = c("06", "07", "08", "09"), # June through September
#'   months_name = "monsoon"
#' )
#' }
htr_seasonal_frequency <- function(indir,
                                   tempdir,
                                   outdir,
                                   months, # define season (in numbered format)
                                   months_name, # define season name for the filename's suffix
                                   overwrite = FALSE, # if TRUE, overwrite existing files
                                   ncores = NULL, # Use all available. Ignored on HPC
                                   hpc = NULL, # if run in the HPC, possible values are "array", "parallel"
                                   file = NULL, # hpc = "array", the input will be the file
                                   cdo_flags = "-f nc4c -z zip_1"
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Create temporary folder if it doesn't exist
  htr_make_folder(tempdir)

  # Define workers
  w <- htr_workers(ncores, hpc)

  ##############

  change_seasons <- function(f, overwrite) {

    basename <- f %>%
      basename() %>%
      stringr::str_split("_merged_") %>%
      purrr::map(~paste0(.x[1], "_seasonal_", .x[2])) %>%
      stringr::str_split("[.]") %>%
      purrr::map(~paste0(.x[1], "_", months_name, ".", .x[2]))

    out_file <- paste0(outdir, "/", basename)

    system(paste0("cdo selmon,", paste0(months, collapse = ","), " ", f, " ", tempdir, "/", basename)) # select only the months that are part of the defined season
    cdo_code <- paste0("cdo ", cdo_flags, " yearmonmean ", tempdir, "/", basename, " ", out_file) # take the yearly mean across the predefined seasons
    htr_run_cdo(cdo_code, out_file, overwrite)

  }

  ##############

  if (isTRUE(hpc %in% "array")) { # For hpc == "array", use the specific files as the starting point

    esm <- htr_list_files(indir, pattern = file)
    if (is.null(esm)) return(invisible(NULL))

    change_seasons(esm, overwrite) # run function

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

    esms <- htr_list_files(indir, pattern = "\\.nc$")
    if (is.null(esms)) return(invisible(NULL))

    future::plan(future::multisession, workers = w)
    furrr::future_walk(esms, change_seasons, overwrite)
    future::plan(future::sequential)
  }

}
