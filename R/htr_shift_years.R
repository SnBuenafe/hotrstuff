#' Shift time coordinates in climate model data
#'
#' This function adjusts time coordinates in climate model files, particularly useful
#' for paleoclimate data where model years need to be shifted to correspond to actual
#' calendar years. It uses CDO (Climate Data Operators) to shift time coordinates
#' and updates filenames accordingly.
#'
#' @details
#' Some climate models, particularly paleoclimate simulations, use arbitrary year
#' numbering that doesn't correspond to actual calendar years. This function shifts
#' the time coordinates to align with real calendar years for proper temporal analysis.
#'
#' The function:
#' 1. Extracts metadata from input filenames to determine current year ranges
#' 2. Checks if years are less than 1200 (indicating they need adjustment)
#' 3. For files needing adjustment:
#'    - Calculates new year ranges by adding the adjustment value
#'    - Uses `cdo shifttime,Nyears` to shift the time coordinates
#'    - Updates filenames with the new year ranges
#' 4. For files not needing adjustment: Simply copies them to the output directory
#'
#' The CDO command used for shifting is:
#' `cdo shifttime,adjust_value years input_file output_file`
#'
#' @author Tin Buenafe
#'
#' @param indir Character string. Directory containing input NetCDF files with
#'   time coordinates that may need shifting.
#' @param outdir Character string. Directory where time-adjusted files will be saved.
#' @param adjust_value Numeric. Number of years to add to the time coordinates.
#'   Can be positive or negative. For example, use 1653 to shift model years
#'   347-1006 to calendar years 2000-2659.
#' @param overwrite Logical. If `FALSE` (default), skips files that already exist
#'   in the output directory. If `TRUE`, regenerates files even if they exist.
#'
#' @return
#' No return value. The function creates time-adjusted files in the specified output
#' directory. Files with years < 1200 are shifted and renamed with new year ranges,
#' while others are copied unchanged.
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Input files must follow CMIP6 naming conventions for proper metadata extraction
#' - The threshold of 1200 years is used to identify files needing time adjustment
#' - Uses parallel processing with (number of CPU cores - 2) workers
#' - Filenames are automatically updated to reflect the new time ranges
#' - Progress messages show which files are being processed
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO shifttime operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=143
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Shift paleoclimate model years to modern calendar years
#' htr_shift_years(
#'   indir = file.path(base_dir, "data", "proc", "regridded", "yearly", "tos"),
#'   outdir = file.path(base_dir, "data", "proc", "shifted", "yearly", "tos"),
#'   adjust_value = 1653  # Shift model years 347-1006 to 2000-2659
#' )
#'
#' # Shift backwards (negative adjustment)
#' htr_shift_years(
#'   indir = file.path(base_dir, "data", "proc", "regridded", "yearly", "tos"),
#'   outdir = file.path(base_dir, "data", "proc", "shifted", "yearly", "tos"),
#'   adjust_value = -50  # Shift 50 years backwards
#' )
#' }
htr_shift_years <- function(indir,
                            outdir,
                            adjust_value,
                            overwrite = FALSE # if TRUE, overwrite existing files
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  w <- parallel::detectCores()-2 # get number of workers

  f <- htr_list_files(indir)
  if (is.null(f)) return(invisible(NULL))

  ######

  do_shift <- function(f, overwrite) {

    meta <- htr_get_CMIP6_bits(f) # get the metadata

    yr_start <- stringr::str_split(meta$Year_start, pattern = "-") %>%
      unlist() # get start year

    yr_end <- stringr::str_split(meta$Year_end, pattern = "-") %>%
      unlist() # get end year

    if(as.numeric(yr_start[1]) < 1200) { # if year < 1200; the dates need to be adjusted
      # Adjust years
      yr1 = as.numeric(yr_start[1]) + adjust_value
      yr2 = as.numeric(yr_end[1]) + adjust_value

      filename <- paste0(paste(meta$Variable,
                               meta$Frequency,
                               meta$Model,
                               meta$Scenario,
                               meta$Variant,
                               meta$Grid,
                               paste0(yr1,
                                      yr_start[2],
                                      yr_start[3]),
                               sep = "_"),
                         "-",
                         paste0(yr2,
                                yr_end[2],
                                yr_end[3]),
                         ".nc")

      # Shift the time using the shifttime function of cdo
      out_file <- paste0(outdir, "/", filename)
      cdo_code <- paste0("cdo shifttime,", adjust_value, "years", " ", f, " ", out_file)
      htr_run_cdo(cdo_code, out_file, overwrite)

    } else {
      filename <- paste(meta$Variable,
                        meta$Frequency,
                        meta$Model,
                        meta$Scenario,
                        meta$Variant,
                        meta$Grid,
                        paste0(paste0(yr_start,
                                      collapse = ""),
                               "-",
                               paste0(yr_end, collapse = ""),
                               ".nc"),
                        sep = "_")
      out_file <- paste0(outdir, "/", filename)
      if (file.exists(out_file) && !overwrite) {
        message("Skipped (exists): ", filename, " - Set overwrite = TRUE to regenerate")
      } else {
        system(paste0("cp ", f, " ", out_file))
        message("Written: ", filename)
      }
    }

    ######

  }

  future::plan(future::multisession, workers = w) # to do the shift in parallel
  furrr::future_walk(f, do_shift, overwrite)
  future::plan(future::sequential) # revert back to sequential processing

}



