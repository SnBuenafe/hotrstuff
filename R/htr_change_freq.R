#' Change frequency
#'
#' @author Tin Buenafe
#'
#' @inheritParams htr_slice_period
#'
#' @export
#'
#' @examples
htr_change_freq <- function(freq,
                            indir,
                            outdir) {
  . <- NULL # Stop devtools::check() complaints about NSE


  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  w <- parallel::detectCores() - 2

  esms <- dir(indir, pattern = "*.nc", full.names = TRUE)

  ##############

  change_yearly <- function(f, outdir) {
    out_file <- f %>%
      basename() %>%
      stringr::str_split("_merged_") %>%
      purrr::map(~ paste0(.x[1], "_annual_", .x[2])) %>%
      paste0(outdir, "/", .)

    cdo_code <- paste0("cdo -yearmean", " ", f, " ", out_file)

    system(cdo_code)
  }

  ##############

  change_monthly <- function(f, outdir) {
    out_file <- f %>%
      basename() %>%
      stringr::str_split("_merged_") %>%
      purrr::map(~ paste0(.x[1], "_monthly_", .x[2])) %>%
      paste0(outdir, "/", .)

    cdo_code <- paste0("cdo -monmean", " ", f, " ", out_file)

    system(cdo_code)
  }

  ##############

  future::plan(future::multisession, workers = w)

  if (stringr::str_to_lower(freq) == "yearly") {
    furrr::future_walk(esms, change_yearly, outdir) # JDE
  } else if (stringr::str_to_lower(freq) == "monthly") {
    furrr::future_walk(esms, change_monthly, outdir) # JDE
  }

  future::plan(future::sequential)
}
