#' Calculate mean of specified time period.
#'
#' Used to calculate baseline means.
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @param indir
#' @param outdir
#' @param scenario
#' @param year_start
#' @param year_end
#'
#' @return
#' @export
#'
#' @examples
htr_calc_mean <- function(indir, # where inputs are
                          outdir, # where outputs will be saved
                          scenario, # historical or ssp (use historical for calculating baseline means)
                          year_start, # start year for calculating mean of time period
                          year_end # end year for calculating mean of time period
) {
  w <- parallel::detectCores() - 2

  esms <- dir(indir, pattern = scenario, full.names = TRUE)
  future::plan(future::multisession, workers = w)
  furrr::future_walk(esms, get_mean)
  future::plan(future::sequential)
}


#' Calculate Mean
#'
#' @param f
#'
#' @return
#'
#' @noRd
#'
get_mean <- function(f) {
  out_file <- f %>%
    basename() %>%
    stringr::str_split("_merged_") %>%
    purrr::map(~ paste0(.x[1], "_mean_", year_start, "0101-", year_end, "1231.nc")) %>%
    paste0(outdir, "/", .)
  cdo_code <- paste0("cdo -L -timmean -selyear,", year_start, "/", year_end, " ", f, " ", out_file)
  system(cdo_code)
}
