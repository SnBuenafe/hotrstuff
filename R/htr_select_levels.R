#' Select vertical levels from depth-resolved climate data by index
#'
#' This function selects specific vertical levels from 3D ocean climate model data
#' using CDO (Climate Data Operators), based on level index numbers rather than
#' actual depth or pressure values. The selected levels are saved as a new NetCDF
#' file without any vertical integration.
#'
#' @details
#' The function processes depth-resolved ocean data (e.g., temperature, salinity)
#' and extracts a subset of vertical levels by their index position (1-based).
#' This is useful when you want to work with specific model levels regardless of
#' their actual depth or pressure coordinate values.
#'
#' The CDO operation performed is:
#' - `cdo sellevidx,idx1,idx2,...` to extract the specified level indices
#'
#' The function:
#' 1. Selects specific vertical levels by index using CDO sellevidx operator
#' 2. Saves the result as a new NetCDF file (no vertical integration is performed)
#' 3. Adds an optional domain name suffix to output filenames
#'
#' @inheritParams htr_seasonal_frequency
#' @param levidx Integer vector. The level index numbers (1-based) to select from
#'   the input data. For example, `c(1, 2, 3)` selects the first three vertical
#'   levels. Use [htr_show_levels()] to inspect available levels and their indices.
#' @param domain_name Character string. Optional suffix to add to output filenames
#'   to identify the selected levels (e.g., "lev1-3", "surface"). Default is empty string.
#' @param overwrite Logical. If `FALSE` (default), skips files that already exist
#'   in the output directory. If `TRUE`, regenerates files even if they exist.
#'
#' @return
#' No return value. The function creates new NetCDF files containing only the
#' selected vertical levels in the specified output directory. If `domain_name`
#' is provided, it is added as a suffix to the original filename before the
#' file extension.
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Input files must be 3D ocean data with vertical levels (depth or pressure coordinates)
#' - Level indices are 1-based and correspond to the order of levels in the file
#' - Use [htr_show_levels()] to inspect the available levels and their indices
#' - Uses parallel processing when `hpc` is not set to "array"
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO sellevidx operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=123
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Select the first 3 vertical levels
#' htr_select_levels(
#'   indir = "path/to/3d/data",
#'   outdir = "path/to/output",
#'   levidx = c(1, 2, 3),
#'   domain_name = "top3levels"
#' )
#'
#' # Select a single level by index
#' htr_select_levels(
#'   indir = "path/to/3d/data",
#'   outdir = "path/to/output",
#'   levidx = 1,
#'   domain_name = "surface"
#' )
#' }
htr_select_levels <- function(indir,
                               outdir,
                               levidx,
                               domain_name = "",
                               overwrite = FALSE, # if TRUE, overwrite existing files
                               ncores = NULL, # Use all available. Ignored on HPC
                               hpc = NULL, # if run in the HPC, possible values are "array", "parallel"
                               file = NULL, # hpc = "array", the input will be the file
                               cdo_flags = "-f nc4c -z zip_1"
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Define workers
  w <- htr_workers(ncores, hpc)

  do_select <- function(f, overwrite) {

    if (stringr::str_length(domain_name) > 0) {
      outname <- f %>%
        basename() %>%
        stringr::str_split("[.]") %>%
        purrr::map(~paste0(.x[1], "_", domain_name, ".", .x[2]))
    } else {
      outname <- f %>%
        basename()
    }

    out_file <- paste0(outdir, "/", outname)

    # Build comma-separated list of level indices for CDO sellevidx
    levidx_str <- paste(levidx, collapse = ",")

    cdo_code <- paste0("cdo ", cdo_flags, " sellevidx,", levidx_str, " ", f, " ", out_file)

    htr_run_cdo(cdo_code, out_file, overwrite)

  }

  if (isTRUE(hpc %in% "array")) { # For hpc == "array", use the specific files as the starting point

    esms <- htr_list_files(indir, pattern = file)
    if (is.null(esms)) return(invisible(NULL))

    do_select(esms, overwrite) # run function

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

    esms <- htr_list_files(indir, pattern = "\\.nc$")
    if (is.null(esms)) return(invisible(NULL))

    future::plan(future::multisession, workers = w)
    furrr::future_walk(esms, do_select, overwrite)
    future::plan(future::sequential)

  }

}
