#' Calculate anomalies relative to the baseline mean
#'
#' @param indir
#' @param mndir
#' @param outdir
#'
#' @return
#' @export
#'
#' @examples
htr_calc_anomalies <- function(indir, # input directory of the projections
                               mndir, # directory of baseline mean
                               outdir # where anomalies will be saved
) {

  w <- parallel::detectCores() - 2

  # get metadata from the files in the baseline directory
  x <- htr_get_meta(mndir,
    string = c("Variable", "Frequency", "Model"))

  ##############

  do_anom <- function(v, fr, m) {
    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", v, "_", ")(?=.*", fr, "_", ")(?=.*", m, "_", ")")) # For each combination of variable-frequency-model that we have a baseline mean for, find merged files for all time periods

    ##############
    subtract_mean <- function(f) {
      bits <- basename(f) %>%
        htr_get_CMIP6_bits()
      mn <- dir(mndir, pattern = paste0(bits$Variable, "_", bits$Frequency, "_", bits$Model)) %>%
        paste0(mndir, "/", .)
      anom_out <- stringr::str_replace_all(f, dirname(f), outdir) %>%
        stringr::str_replace("_merged_", "_anomalies_")
      cdo_code <- paste0("cdo sub ", f, " ", mn, " ", anom_out)
      system(cdo_code)
    }
    ##############

    future::walk(files, subtract_mean)
  }

  ##############

  future::plan(future::multisession, workers = w)
  furrr::future_pwalk(x, do_anom)
  future::plan(future::sequential)
}


