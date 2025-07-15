#' Calculate vertical means from depth-resolved climate data
#'
#' This function calculates vertical (depth) means from 3D ocean climate model data
#' using CDO (Climate Data Operators). It can either integrate across all vertical
#' levels or select a specific depth range before calculating the vertical mean.
#'
#' @details
#' The function processes depth-resolved ocean data (e.g., temperature, salinity)
#' to create vertically-averaged fields. This is useful for analyzing ocean properties
#' at specific depth ranges or creating depth-integrated quantities.
#'
#' The CDO operations performed are:
#' - **With level selection**: `cdo select,levrange=min,max` followed by `cdo vertmean`
#' - **Without level selection**: `cdo vertmean` directly on the full depth range
#'
#' The function:
#' 1. Optionally selects a specific depth range using CDO select with levrange
#' 2. Calculates the vertical mean using CDO vertmean operator
#' 3. Adds an optional domain name suffix to output filenames
#' 4. Uses a temporary directory for intermediate processing when level selection is used
#'
#' @inheritParams htr_seasonal_frequency
#' @param tempdir Character string. Directory for temporary files during processing.
#'   Used when `select_levels = TRUE` to store intermediate files after level selection.
#' @param select_levels Logical. If `TRUE`, selects a specific depth range defined by
#'   `min_level` and `max_level` before calculating vertical means. If `FALSE` (default),
#'   integrates across all available vertical levels.
#' @param min_level Numeric. Minimum depth level for integration (required when
#'   `select_levels = TRUE`). Units depend on the model's vertical coordinate system.
#' @param max_level Numeric. Maximum depth level for integration (required when
#'   `select_levels = TRUE`). Units depend on the model's vertical coordinate system.
#' @param domain_name Character string. Optional suffix to add to output filenames
#'   to identify the depth domain (e.g., "surface", "0-100m"). Default is empty string.
#'
#' @return
#' No return value. The function creates vertically-integrated files in the specified
#' output directory. If `domain_name` is provided, it is added as a suffix to the
#' original filename before the file extension.
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Input files must be 3D ocean data with vertical levels (depth or pressure coordinates)
#' - Temporary files are automatically cleaned up after processing
#' - Uses parallel processing when `hpc` is not set to "array"
#' - Level selection uses CDO's levrange which works with the model's native vertical coordinates
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO vertmean operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=203
#' CDO select operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=123
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Integrate all levels
#' htr_integrate_levels(
#'   indir = "path/to/3d/data",
#'   tempdir = "path/to/temp",
#'   outdir = "path/to/output",
#'   select_levels = FALSE
#' )
#'
#' # Integrate specific depth range (e.g., upper 100m)
#' htr_integrate_levels(
#'   indir = "path/to/3d/data",
#'   tempdir = "path/to/temp",
#'   outdir = "path/to/output",
#'   select_levels = TRUE,
#'   min_level = 0,
#'   max_level = 100,
#'   domain_name = "upper100m"
#' )
#' }
htr_integrate_levels <- function(hpc = NA, # if ran in the HPC, possible values are "array", "parallel"
                                 file = NA, # hpc = "array", the input will be the file
                                 indir,
                                 tempdir,
                                 outdir,
                                 select_levels = FALSE, # if NA, then integrate all levels
                                 min_level,
                                 max_level,
                                 domain_name = ""
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Create temporary folder if it doesn't exist
  htr_make_folder(tempdir)

  # Define workers
  if(is.na(hpc)) {
    w <- parallelly::availableCores(method = "system", omit = 2)
  } else {
    w <- parallelly::availableCores(method = "Slurm", omit = 2)
  }

  do_integrate <- function(f) {

    if(stringr::str_length(domain_name) > 0) {
      outname <- f %>%
        basename() %>%
        stringr::str_split("[.]") %>%
        purrr::map(~paste0(.x[1], "_", domain_name, ".", .x[2]))
    } else {
      outname <- f %>%
        basename()
    }


    out_file <- paste0(outdir, "/", outname)

    if(isTRUE(select_levels)) {
      system(paste0("cdo select,levrange=", min_level, ",", max_level, " ", f, " ", tempdir, "/", basename(f)))
      system(paste0("cdo vertmean ", tempdir, "/", basename(f), " ", out_file))
    } else {
      system(paste0("cdo vertmean ", f, " ", out_file))
    }

    print(basename(f))

  }

  if (hpc %in% c("array")) { # For hpc == "array", use the specific files as the starting point

    esms <- dir(indir, pattern = file, full.names = TRUE)

    do_integrate(esms) # run function

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

    esms <- dir(indir, pattern = "*.nc", full.names = TRUE)

    future::plan(future::multisession, workers = w)
    furrr::future_walk(esms, do_integrate)
    future::plan(future::sequential)

  }

  # Delete temporary files
  system(paste0("rm ", tempdir, "/*"))

}
