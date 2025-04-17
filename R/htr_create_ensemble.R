#' Create an ensemble based on list of models
#'
#'
#' @inheritParams htr_slice_period
#' @param model_list Character string of models to use for the ensemble
#' @param variable The variable to create the ensemble for
#' @param mean Use the mean (TRUE; default) or the median (FALSE) when creating the ensemble.
#' @param season If using seasonal frequency, input the season name to detect the files
#' @param domain If using depth-resolved models, input the domain name to detect the files
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_create_ensemble(
#' hpc = NA,
#' indir = file.path(base_dir, "data", "proc", "regridded", "yearly", "tos"),
#' outdir = file.path(base_dir, "data", "proc", "ensemble", "mean", "tos"),
#' model_list = c("ACCESS-ESM1-5", "CanESM5"),
#' variable = "tos",
#' freq = "Omon",
#' scenario = "ssp126",
#' mean = TRUE
#' )
#' }
htr_create_ensemble <- function(hpc = NA, # if ran in the HPC, possible values are "array", "parallel"
                                indir,
                                outdir,
                                model_list,
                                variable = "tos",
                                freq = "Omon",
                                scenario = "historical",
                                season = "", # default is no season
                                domain = "", # default is no domain
                                mean = TRUE # if false, use median
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Define workers
  if(is.na(hpc)) {
    w <- parallelly::availableCores(methods = "system", omit = 2)
  } else {
    w <- parallelly::availableCores(methods = "Slurm", omit = 2)
  }

  ##############

  if (stringr::str_length(domain) > 0 & stringr::str_length(season) > 0) {

    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", variable, "_", ")(?=.*", freq, "_", ")(?=.*", scenario, "_", ")(?=.*", season, "_", ")(?=.*", domain, ")")) %>%
      stringr::str_subset(paste(model_list, collapse = "|"))

  } else if (stringr::str_length(domain) > 0) {

    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", variable, "_", ")(?=.*", freq, "_", ")(?=.*", scenario, "_", ")(?=.*",  domain, ")")) %>%
      stringr::str_subset(paste(model_list, collapse = "|"))

  } else if (stringr::str_length(season) > 0) {

    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", variable, "_", ")(?=.*", freq, "_", ")(?=.*", scenario, "_", ")(?=.*",  season, ")")) %>%
      stringr::str_subset(paste(model_list, collapse = "|"))

  } else {

    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", variable, "_", ")(?=.*", freq, "_", ")(?=.*", scenario, ")")) %>%
      stringr::str_subset(paste(model_list, collapse = "|"))

  }

  out_name <- files[1] %>%
    stringr::str_replace(indir, outdir) %>%
    stringr::str_replace(htr_get_CMIP6_bits(files[1])$Model, "ensemble")

  ##############

  if (mean == TRUE) {

    cdo_code <- paste0("cdo -L -z zip -ensmean ", paste0(files, collapse = " "), " ", out_name)

  } else if (mean == FALSE) { # Calculate the median

    cdo_code <- paste0("cdo -L -z zip -ensmedian ", paste0(files, collapse = " "), " ", out_name)

  } else {

    print("Please provide the right option for mean")

  }

  system(cdo_code)

}
