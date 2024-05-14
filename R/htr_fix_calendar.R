#' Fix calendars (leap years)
#'
#' pacman::p_load(tidyverse, ncdf4, furrr)
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @param indir
#'
#' @return
#' @export
#'
#' @examples
htr_fix_calendar <- function(indir # input directory
) {
  w <- parallel::detectCores() - 2

  fix_cal <- function(f) {
    yrs <- ncdf4::nc_open(f) %>%
      ncdf4::ncvar_get(., "time") %>%
      length(.) %% 365 # Modulo...returns zero if number of days divides by 365 without remainder
    if (yrs != 0 & stringr::str_detect(htr_get_CMIP6_bits(basename(f))$Frequency, "day_")) {
      cat(paste0(basename(f), " DOES have leap days to be removed"))
      cat("\n")
      system(paste0("cdo -L -setcalendar,365_day -delete,month=2,day=29 ", f, " ", dirname(f), "/tmp_", basename(f)))
      system(paste0("rm ", f))
      file.rename(paste0(dirname(f), "/tmp_", basename(f)), f)
    } else {
      cat(paste0(basename(f), " does not have leap days"))
      cat("\n")
      system(paste0("cdo setcalendar,365_day ", f, " ", dirname(f), "/tmp_", basename(f)))
      system(paste0("rm ", f))
      file.rename(paste0(dirname(f), "/tmp_", basename(f)), f)
    }
  }


  netCDFs <- dir(indir, full.names = TRUE)
  future::plan(future::multisession, workers = w)
  furrr::future_walk(netCDFs, fix_cal)
  future::plan(future::sequential)
}


