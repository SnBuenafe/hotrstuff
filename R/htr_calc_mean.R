#' Calculate mean of specified time period.
#'
#' Used to calculate baseline means.
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @inheritParams htr_slice_period
#'
#' @export
#'
#' @examples
htr_calc_mean <- function(indir, # where inputs are
                          outdir, # where outputs will be saved
                          scenario, # historical or ssp (use historical for calculating baseline means)
                          year_start, # start year for calculating mean of time period
                          year_end # end year for calculating mean of time period
) {
  . <- NULL # Stop devtools::check() complaints about NSE

  w <- parallel::detectCores() - 2

  ##############

  get_mean <- function(f) {
    out_file <- f %>%
      basename() %>%
      stringr::str_split("_merged_") %>%
      purrr::map(~ paste0(.x[1], "_mean_", year_start, "0101-", year_end, "1231.nc")) %>%
      paste0(outdir, "/", .)
    cdo_code <- paste0("cdo -L -timmean -selyear,", year_start, "/", year_end, " ", f, " ", out_file)
    system(cdo_code)
  }

  ##############

  esms <- dir(indir, pattern = scenario, full.names = TRUE)
  future::plan(future::multisession, workers = w)
  furrr::future_walk(esms, get_mean)
  future::plan(future::sequential)
}
