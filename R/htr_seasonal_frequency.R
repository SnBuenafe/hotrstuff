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
#' htr_seasonal_frequency(
#' hpc = NA,
#' file = NA,
#' indir = here("data", "proc", "sliced", "omip", variable),
#' tempdir = here("data", "temporary"),
#' outdir = here("data", "proc", "seasonal", "omip", variable),
#' months = c("01", "02", "03"), # define season (in numbered format)
#' months_name = "jan-mar" # define season name
#' )
#' }
htr_seasonal_frequency <- function(hpc = NA, # if ran in the HPC, possible values are "array", "parallel"
                                   file = NA, # hpc = "array", the input will be the file
                                   indir,
                                   tempdir,
                                   outdir,
                                   months, # define season (in numbered format)
                                   months_name # define season name for the filename's suffix
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

  if(hpc == "array") { # For hpc == "array", use the specific files as the starting point
    esm <- dir(indir, pattern = file, full.names = TRUE)
    change_seasons(esm) # run function
  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel
    esms <- dir(indir, pattern = "*.nc", full.names = TRUE)
    future::plan(future::multisession, workers = w)
    furrr::future_walk(esms, change_seasons)
    future::plan(future::sequential)
  }

}
