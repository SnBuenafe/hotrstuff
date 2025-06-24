#' Get the weighted vertical means
#'
#' hjfjhfjhf
#'
#' @inheritParams htr_seasonal_frequency
#' @param select_levels If select levels = TRUE, minimum and maximum levels need to be provided
#' @param min_level Minimum level of depth domain
#' @param max_level Maximum level of depth domain
#' @param domain_name Depth domain name
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
htr_integrate_levels <- function(hpc = NA, # if ran in the HPC, possible values are "array", "parallel"
                                 file = NA, # hpc = "array", the input will be the file
                                 indir,
                                 tempdir,
                                 outdir,
                                 select_levels = FALSE, # if NA, then integrate all levels
                                 min_level,
                                 max_level,
                                 domain_name = ""
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Create temporary folder if it doesn't exist
  htr_make_folder(tempdir)

  # Define workers
  if(is.na(hpc)) {
    w <- parallelly::availableCores(method = "system", omit = 2)
  } else {
    w <- parallelly::availableCores(method = "Slurm", omit = 2)
  }

  do_integrate <- function(f) {

    if(stringr::str_length(domain_name) > 0) {
      outname <- f %>%
        basename() %>%
        stringr::str_split("[.]") %>%
        purrr::map(~paste0(.x[1], "_", domain_name, ".", .x[2]))
    } else {
      outname <- f %>%
        basename()
    }


    out_file <- paste0(outdir, "/", outname)

    if(isTRUE(select_levels)) {
      system(paste0("cdo select,levrange=", min_level, ",", max_level, " ", f, " ", tempdir, "/", basename(f)))
      system(paste0("cdo vertmean ", tempdir, "/", basename(f), " ", out_file))
    } else {
      system(paste0("cdo vertmean ", f, " ", out_file))
    }

    print(basename(f))

  }

  if (hpc %in% c("array")) { # For hpc == "array", use the specific files as the starting point

    esms <- dir(indir, pattern = file, full.names = TRUE)

    do_integrate(esms) # run function

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

    esms <- dir(indir, pattern = "*.nc", full.names = TRUE)

    future::plan(future::multisession, workers = w)
    furrr::future_walk(esms, do_integrate)
    future::plan(future::sequential)

  }

  # Delete temporary files
  system(paste0("rm ", tempdir, "/*"))

}
