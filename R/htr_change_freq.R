#' Change frequency
#'
#' @author Tin Buenafe
#'
#' @param freq
#' @param indir
#' @param outdir
#'
#' @return
#' @export
#'
#' @examples
htr_change_freq <- function(freq,
                            indir,
                            outdir) {

  w <- detectCores()-2

  esms <- dir(indir, pattern = "*.nc", full.names = TRUE)
  plan(multisession, workers = w)

  if (stringr::str_to_lower(freq) == "yearly") {

    future_walk(esms, change_yearly)

  } else if (stringr::str_to_lower(freq) == "monthly") {

    future_walk(esms, change_monthly)
  }

  plan(sequential)

}




#' Change to yearly
#'
#' @param f
#'
#' @return
change_yearly <- function(f) {

  out_file <- f %>%
    basename() %>%
    str_split("_merged_") %>%
    map(~paste0(.x[1], "_annual_", .x[2])) %>%
    paste0(outdir, "/", .)

  cdo_code <- paste0("cdo -yearmean", " ", f, " ", out_file)

  system(cdo_code)

}

#' Change to monthly
#'
#' @param f
#'
#' @return
change_monthly <- function(f) {

  out_file <- f %>%
    basename() %>%
    str_split("_merged_") %>%
    map(~paste0(.x[1], "_monthly_", .x[2])) %>%
    paste0(outdir, "/", .)

  cdo_code <- paste0("cdo -monmean", " ", f, " ", out_file)

  system(cdo_code)

}
