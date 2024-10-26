#' Shift years
#'
#' @author Tin Buenafe
#'
#' @inheritParams htr_slice_period
#' @param adjust_value Years that will be used to adjust the time (could be positive or negative)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_shift_years(
#' indir = file.path(base_dir, "data", "proc", "regridded", "yearly", "tos"),
#' outdir = file.path(base_dir, "data", "proc", "ensemble", "mean", "tos"),
#' adjust_value = 1653
#' )
#' }
htr_shift_years <- function(indir,
                            outdir,
                            adjust_value
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  w <- parallel::detectCores()-2 # get number of workers

  f <- dir(indir, full.names = TRUE)

  ######

  do_shift <- function(f) {

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
      system(paste0("cdo shifttime,", adjust_value, "years", " ", f, " ", outdir, "/", filename))

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
      system(paste0("cp ", f, " ", outdir, "/", filename))
      print(filename)
    }

    ######

  }

  future::plan(future::multisession, workers = w) # to do the shift in parallel
  furrr::future_walk(f, do_shift)
  future::plan(future::sequential) # revert back to sequential processing

}



