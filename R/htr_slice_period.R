#' Slice Period
#'
#' pacman::p_load(furrr, tidyverse, parallel)
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @param indir
#' @param outdir
#' @param frq
#' @param scenario
#' @param year_start
#' @param year_end
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
httr_slice_period <- function(indir, # where the merged files are
                              outdir, # where the trimmed files will be saved
                              frq, # frequency
                              scenario, # historical or ssp
                              year_start,
                              year_end,
                              overwrite # TRUE or FALSE
) {
  w <- parallel::detectCores() - 2

  files <- dir(indir, pattern = paste0("_", frq, "_"))

  files <- files[stringr::str_detect(files, scenario)]

  trim_timeframe <- function(f) {
    s <- get_CMIP6_bits(f)$Scenario
    m <- get_CMIP6_bits(f)$Model

    print(paste0(m, "_", s))

    if (stringr::str_detect(s, scenario)) {
      trim_period(
        f,
        s,
        indir,
        outdir,
        year_start,
        year_end,
        overwrite
      )
    } else {
      print(paste0("Scenario ", s, " was not chosen"))
    }
  }

  future::plan(future::multisession, workers = w)
  future::future_walk(files, trim_timeframe)
  future::plan(future::sequential)
}






#' Trim time frame based on start and end months
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @param f
#' @param scenario
#' @param indir
#' @param outdir
#' @param year_start
#' @param year_end
#' @param overwrite
#'
#' @return
#'
#' @noRd
trim_period <- function(f, # file
                        scenario, # historical or ssp
                        indir,
                        outdir,
                        year_start,
                        year_end,
                        overwrite) {
  dt1 <- get_CMIP6_bits(f)$Year_start %>%
    as.Date()

  dt2 <- get_CMIP6_bits(f)$Year_end %>%
    as.Date()

  if (dt1 <= as.Date(paste0(year_start, "-01-01")) | dt2 >= as.Date(paste0(year_end, "-12-31"))) {
    get_Years(f, year_start, year_end, indir, outdir, overwrite) # replacing files in merged folder with trimmed files

    if (isTRUE(overwrite)) {
      terminal_code <- paste0("rm ", indir, "/", f)
      system(terminal_code)
    }
  }
}
