#' Fix calendars (leap years)
#'
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @inheritParams htr_slice_period
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' htr_fix_calendar(
#'   hpc = NA,
#'   file = NA,
#'   indir = file.path(base_dir, "data", "merged"), # input directory
#' )
#' }
htr_fix_calendar <- function(hpc = NA, # if ran in the HPC, possible values are "array", "parallel"
                             file = NA, # hpc = "array", the input will be the file
                             indir) { # input directory

  . <- NULL # Stop devtools::check() complaints about NSE

  # Define workers
  if(is.na(hpc)) {
    w <- parallelly::availableCores(method = "system", omit = 2)
  } else {
    w <- parallelly::availableCores(method = "Slurm", omit = 2)
  }

  ##############

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

  ##############

  if (hpc %in% c("array")) { # For hpc == "array", use the specific files as the starting point

    file <- dir(indir, pattern = file, full.names = TRUE)

    fix_cal(file) # run function

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

    netCDFs <- dir(indir, full.names = TRUE)

    future::plan(future::multisession, workers = w)
    furrr::future_walk(netCDFs, fix_cal) # run function in parallel
    future::plan(future::sequential)

  }

}
