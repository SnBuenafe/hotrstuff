#' Create an ensemble based on list of models
#'
#' pacman::p_load(here, tidyverse, parallel, furrr)
#'
#' @param indir
#' @param outdir
#' @param model_list
#' @param variable
#' @param frequency
#' @param scenario
#' @param mean
#'
#' @return
#' @export
#'
#' @examples
htr_create_ensemble <- function(indir,
                                outdir,
                                model_list,
                                variable = "tos",
                                frequency = "Omon",
                                scenario = "historical",
                                mean = TRUE # if false, use median
) {
  w <- parallel::detectCores() - 2

  files <- dir(indir, full.names = TRUE) %>%
    stringr::str_subset(paste0("(?=.*", variable, "_", ")(?=.*", frequency, "_", ")(?=.*", scenario, "_", ")")) %>%
    stringr::str_subset(paste(model_list, collapse = "|"))

  out_name <- files[1] %>%
    stringr::str_replace(indir, outdir) %>%
    stringr::str_replace(htr_get_CMIP6_bits(files[1])$Model, "ensemble")

  if (mean == TRUE) {
    cdo_code <- paste0("cdo -L -z zip -ensmean ", paste0(files, collapse = " "), " ", out_name)
  } else if (mean == FALSE) { # Calculate the median
    cdo_code <- paste0("cdo -L -z zip -ensmedian ", paste0(files, collapse = " "), " ", out_name)
  } else {
    print("Please provide the right option for mean")
  }

  system(cdo_code)
}
