#' Get the weighted vertical means
#'
#' @inheritParams htr_seasonal_frequency
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
htr_integrate_levels <- function(indir,
                                 tempdir,
                                 outdir,
                                 min_level,
                                 max_level,
                                 domain_name = ""
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Create temporary folder if it doesn't exist
  htr_make_folder(tempdir)

  w <- parallel::detectCores()-2

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

    system(paste0("cdo select,levrange=", min_level, ",", max_level, " ", f, " ", tempdir, "/", basename(f)))

    system(paste0("cdo vertmean ", tempdir, "/", basename(f), " ", out_file))

    print(basename(f))

  }

  esms <- dir(indir, pattern = "*.nc", full.names = TRUE)
  future::plan(future::multisession, workers = w)
  furrr::future_walk(esms, do_integrate)
  future::plan(future::sequential)

}
