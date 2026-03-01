#' Merge climate model files into continuous time series
#'
#' This function merges multiple NetCDF files from the same climate model, variable,
#' frequency, scenario, and variant into single continuous time series files using
#' CDO (Climate Data Operators). This is essential for creating uninterrupted time
#' series from climate model outputs that are often split across multiple files.
#'
#' @details
#' Climate model data is typically provided as multiple files covering different
#' time periods. This function combines these files into continuous time series
#' using the CDO `mergetime` operator, which concatenates files along the time dimension.
#'
#' The function:
#' 1. Extracts metadata (variable, frequency, scenario, model, variant) from all files
#' 2. Groups files by their metadata combinations
#' 3. Filters files based on the specified year range to avoid out-of-scope data
#' 4. Merges files for each group using `cdo -L -selname,'variable' -mergetime`
#' 5. Creates output filenames with "_merged_" and the full time range
#'
#' The CDO command used is:
#' `cdo -L -selname,'variable' -mergetime input_files output_file`
#'
#' Where:
#' - `-L` enables netCDF4 compression
#' - `selname` ensures only the specified variable is retained
#' - `mergetime` concatenates files along the time dimension
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @inheritParams htr_slice_period
#' @param year_start Numeric. Earliest year to include in the merged files. Files
#'   ending before this year (for historical data) will be excluded.
#' @param year_end Numeric. Latest year to include in the merged files. Files
#'   starting after this year (for projection data) will be excluded.
#' @param overwrite Logical. If `FALSE` (default), skips files that already exist
#'   in the output directory. If `TRUE`, regenerates files even if they exist.
#'
#' @return
#' No return value. The function creates merged time series files in the specified
#' output directory with filenames following the pattern:
#' `variable_frequency_model_scenario_variant_merged_YYYYMMDD-YYYYMMDD.nc`
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Input files must follow CMIP6 naming conventions for proper metadata extraction
#' - Files are only merged if they don't already exist in the output directory
#' - Uses parallel processing with (number of CPU cores - 2) workers
#' - The `-L` flag enables netCDF4 compression for smaller output files
#' - Automatically handles different time ranges for historical vs. projection scenarios
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO mergetime operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=102
#' CDO selname operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=126
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get a path to a temporary directory
#' temp_dir <- tempdir()
#'
#' htr_merge_files(
#'   indir = system.file("extdata", package = "hotrstuff"), # input directory
#'   outdir = file.path(temp_dir, "merged"), # output directory
#'   year_start = 1990, # earliest year across all the scenarios considered
#'   year_end = 2014 # latest year across all the scenarios considered
#' )
#' }
htr_merge_files <- function(indir, # where nc files are located
                            outdir, # where merged files should be saved
                            year_start, # start year of historical file
                            year_end, # end year of projection file
                            overwrite = FALSE, # if TRUE, overwrite existing files
                            ncores = NULL, # Use all available. Ignored on HPC
                            hpc = NULL, # if run in the HPC, possible values are "array", "parallel"
                            cdo_flags = "-f nc4c -z zip_1"
) {
  . <- NULL # Stop devtools::check() complaints about NSE

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Check for files in input directory
  all_files <- htr_list_files(indir)
  if (is.null(all_files)) return(invisible(NULL))

  # Define workers
  w <- htr_workers(ncores, hpc)

  l <- htr_get_meta(indir, string = c("Variable", "Frequency", "Scenario", "Model", "Variant"))

  ##############

  do_merge <- function(v, # variable
                       fr, # frequency
                       s, # scenario
                       m, # model
                       vt # variant
  ) {
    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", v, "_", ")(?=.*", fr, "_", ")(?=.*", m, "_", ")(?=.*", s, "_", ")(?=.*", vt, "_", ")")) # in reg exp ".*" means any string of any length, so this formulation requires the variable, model and scenario to be in THAT order in a string, with each followed by "_", but with no other real constraints

    # Ignore any files that don't are out of scope for our start and end years
    if (s == "historical") {
      files <- files %>%
        basename() %>%
        purrr::map(~ htr_get_CMIP6_bits(.x)) %>%
        purrr::map("Year_end") %>% # Get Year_end from each element of the list
        purrr::map(~ ifelse(as.Date(.x) < as.Date(paste0(year_start, "-01-01")), FALSE, TRUE)) %>%
        unlist() %>%
        files[.]
    } else {
      files <- files %>%
        basename() %>%
        purrr::map(~ htr_get_CMIP6_bits(.x)) %>%
        purrr::map("Year_start") %>% # Get Year_start from each element of the list
        purrr::map(~ ifelse(as.Date(.x) > as.Date(paste0(year_end, "-01-01")), FALSE, TRUE)) %>%
        unlist() %>%
        files[.]
    }

    if (length(files) > 0) { # Only if there are files to process
      y1 <- htr_get_CMIP6_bits(files[1])$Year_start %>%
        as.character() %>%
        stringr::str_replace_all("-", "")

      y2 <- htr_get_CMIP6_bits(files[length(files)])$Year_end %>%
        as.character() %>%
        stringr::str_replace_all("-", "")

      out_file <- paste0(
        outdir, "/", v, "_", fr, "_", m, "_", s,
        "_", vt, "_merged_", y1, "-", y2, ".nc"
      )

      cdo_code <- paste0("cdo ", cdo_flags, " -L -selname,", "'", v, "' -mergetime ", paste0(files, collapse = " "), " ", out_file)
      htr_run_cdo(cdo_code, out_file, overwrite)
    }
  }

  ##############

  future::plan(future::multisession, workers = w) # to download wget files in parallel
  furrr::future_pwalk(l, do_merge) # JDE
  future::plan(future::sequential) # revert back to sequential processing
}
