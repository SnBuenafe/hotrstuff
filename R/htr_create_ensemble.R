#' Create an ensemble based on list of models
#'
#' @inheritParams htr_slice_period
#' @param model_list Character string of models to use for the ensemble
#' @param variable The variable to create the ensemble for
#' @param mean Use the mean (TRUE; default) or the median (FALSE) when creating the ensemble.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_create_ensemble(
#' indir = file.path(base_dir, "data", "proc", "regridded", "yearly", "tos"),
#' outdir = file.path(base_dir, "data", "proc", "ensemble", "mean", "tos"),
#' model_list = c("ACCESS-ESM1-5", "CanESM5"),
#' variable = "tos",
#' freq = "Omon",
#' scenario = "ssp126",
#' mean = TRUE
#' )
#' }
htr_create_ensemble <- function(indir,
                                outdir,
                                model_list,
                                variable = "tos",
                                freq = "Omon",
                                scenario = "historical",
                                mean = TRUE # if false, use median
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  w <- parallel::detectCores() - 2

  files <- dir(indir, full.names = TRUE) %>%
    stringr::str_subset(paste0("(?=.*", variable, "_", ")(?=.*", freq, "_", ")(?=.*", scenario, "_", ")")) %>%
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
