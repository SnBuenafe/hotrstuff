#' Change frequency to seasonal frequency
#'
#' @inheritParams htr_slice_period
#' @param tempdir Temporary directory where specific months are selected
#' @param months Define season (based in the numbered format of months)
#' @param months_name Define seasone name for the filename's suffix
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_seasonal_frequency(indir = here("data", "proc", "sliced", "omip", variable),
#' tempdir = here("data", "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
#' outdir = here("data", "proc", "seasonal", "omip", variable),
#' months = c("01", "02", "03"), # define season (in numbered format)
#' months_name = "jan-mar" # define season name
#' )
#' }
htr_seasonal_frequency <- function(indir,
                                   tempdir,
                                   outdir,
                                   months, # define season (in numbered format)
                                   months_name # define season name for the filename's suffix
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Create temporary folder if it doesn't exist
  htr_make_folder(tempdir)

  w <- parallel::detectCores()-2

  change_seasons <- function(f) {

    basename <- f %>%
      basename() %>%
      stringr::str_split("_merged_") %>%
      purrr::map(~paste0(.x[1], "_seasonal_", .x[2])) %>%
      stringr::str_split("[.]") %>%
      purrr::map(~paste0(.x[1], "_", months_name, ".", .x[2]))

    out_file <- paste0(outdir, "/", basename)

    system(paste0("cdo selmon,", paste0(months, collapse = ","), " ", f, " ", tempdir, "/", basename)) # select only the months that are part of the defined season
    system(paste0("cdo yearmonmean ", tempdir, "/", basename, " ", out_file)) # take the yearly mean across the predefined seasons

  }

  esms <- dir(indir, pattern = "*.nc", full.names = TRUE)
  future::plan(future::multisession, workers = w)
  furrr::future_walk(esms, change_seasons)
  future::plan(future::sequential)

}
