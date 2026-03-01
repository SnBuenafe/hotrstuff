#' Create multi-model ensemble from climate model outputs
#'
#' This function creates multi-model ensembles by combining outputs from multiple
#' climate models using CDO (Climate Data Operators). It can calculate either the
#' ensemble mean or median across the specified models, with support for seasonal
#' and depth-resolved data filtering.
#'
#' @details
#' The function uses CDO ensemble operators to combine multiple model outputs:
#' - **Ensemble mean**: Uses `cdo -ensmean` to calculate the arithmetic mean across models
#' - **Ensemble median**: Uses `cdo -ensmedian` to calculate the median across models
#'
#' The function automatically:
#' 1. Filters files based on variable, frequency, scenario, and optionally season/domain
#' 2. Selects only files from the specified models in `model_list`
#' 3. Creates ensemble statistics using the appropriate CDO operator
#' 4. Saves output with "ensemble" replacing the model name in the filename
#'
#' Output files are compressed using zip compression (`-z zip`) and use netCDF4
#' format with the `-L` flag for efficient storage.
#'
#' @inheritParams htr_slice_period
#' @param model_list Character vector. Names of climate models to include in the
#'   ensemble. Model names must match those in the input filenames (e.g.,
#'   `c("ACCESS-ESM1-5", "CanESM5", "GFDL-ESM4")`).
#' @param variable Character string. The climate variable to create the ensemble for
#'   (e.g., "tos" for sea surface temperature, "pr" for precipitation). Default is "tos".
#' @param mean Logical. If `TRUE` (default), calculates ensemble mean using CDO ensmean.
#'   If `FALSE`, calculates ensemble median using CDO ensmedian.
#' @param season Character string. Optional season name to filter files (e.g., "DJF",
#'   "JJA"). Only files containing this string will be included. Default is empty string (no filtering).
#' @param domain Character string. Optional domain name for depth-resolved models
#'   (e.g., "surface", "0-100m"). Only files containing this string will be included.
#'   Default is empty string (no filtering).
#' @param overwrite Logical. If `FALSE` (default), skips files that already exist
#'   in the output directory. If `TRUE`, regenerates files even if they exist.
#'
#' @return
#' No return value. The function creates an ensemble file in the specified output
#' directory with "ensemble" replacing the model name in the original filename.
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - All input files must be on the same spatial grid (use [`htr_regrid_esm()`] first if needed)
#' - All input files must have the same temporal resolution and time periods
#' - Model names in `model_list` must exactly match those in the input filenames
#' - Uses zip compression for efficient file storage
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO ensmean operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=78
#' CDO ensmedian operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=79
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_create_ensemble(
#'   indir = file.path(base_dir, "data", "proc", "regridded", "yearly", "tos"),
#'   outdir = file.path(base_dir, "data", "proc", "ensemble", "mean", "tos"),
#'   model_list = c("ACCESS-ESM1-5", "CanESM5"),
#'   variable = "tos",
#'   freq = "Omon",
#'   scenario = "ssp126",
#'   mean = TRUE
#' )
#' }
htr_create_ensemble <- function(indir,
                                outdir,
                                model_list,
                                variable = "tos",
                                freq = "Omon",
                                scenario = "historical",
                                season = "", # default is no season
                                domain = "", # default is no domain
                                mean = TRUE, # if false, use median
                                overwrite = FALSE, # if TRUE, overwrite existing files
                                ncores = NULL, # Use all available. Ignored on HPC
                                hpc = NULL, # if run in the HPC, possible values are "array", "parallel"
                                cdo_flags = "-f nc4c -z zip_1"
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Check for files in input directory
  all_files <- htr_list_files(indir)
  if (is.null(all_files)) return(invisible(NULL))

  # Define workers
  w <- htr_workers(ncores, hpc)

  ##############

  if (stringr::str_length(domain) > 0 & stringr::str_length(season) > 0) {

    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", variable, "_", ")(?=.*", freq, "_", ")(?=.*", scenario, "_", ")(?=.*", season, "_", ")(?=.*", domain, ")")) %>%
      stringr::str_subset(paste(model_list, collapse = "|"))

  } else if (stringr::str_length(domain) > 0) {

    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", variable, "_", ")(?=.*", freq, "_", ")(?=.*", scenario, "_", ")(?=.*",  domain, ")")) %>%
      stringr::str_subset(paste(model_list, collapse = "|"))

  } else if (stringr::str_length(season) > 0) {

    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", variable, "_", ")(?=.*", freq, "_", ")(?=.*", scenario, "_", ")(?=.*",  season, ")")) %>%
      stringr::str_subset(paste(model_list, collapse = "|"))

  } else {

    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", variable, "_", ")(?=.*", freq, "_", ")(?=.*", scenario, ")")) %>%
      stringr::str_subset(paste(model_list, collapse = "|"))

  }

  out_name <- files[1] %>%
    stringr::str_replace(indir, outdir) %>%
    stringr::str_replace(htr_get_CMIP6_bits(files[1])$Model, "ensemble")

  ##############

  if (mean == TRUE) {

    cdo_code <- paste0("cdo ", cdo_flags, " -L -ensmean ", paste0(files, collapse = " "), " ", out_name)

  } else if (mean == FALSE) { # Calculate the median

    cdo_code <- paste0("cdo ", cdo_flags, " -L -ensmedian ", paste0(files, collapse = " "), " ", out_name)

  } else {

    print("Please provide the right option for mean")
    return(invisible(NULL))

  }

  htr_run_cdo(cdo_code, out_name, overwrite)

}
