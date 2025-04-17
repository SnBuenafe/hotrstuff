#' Change frequency
#'
#' @author Tin Buenafe
#'
#' @inheritParams htr_slice_period
#'
#' @export
#'
#' @examples
#' htr_change_freq(
#' hpc = NA,
#' file = NA,
#' freq = "monthly",
#' indir = here("data", "proc", "sliced", variable),
#' outdir = here("data", "proc", "monthly", variable)
#' )
htr_change_freq <- function(hpc = NA, # if ran in the HPC, possible values are "array", "parallel"
                            file = NA, # hpc = "array", the input will be the file
                            freq, # possible values are "yearly" or "monthly"
                            indir,
                            outdir) {
  . <- NULL # Stop devtools::check() complaints about NSE


  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Define workers
  if(is.na(hpc)) {
    w <- parallelly::availableCores(method = "system", omit = 2)
  } else {
    w <- parallelly::availableCores(method = "Slurm", omit = 2)
  }

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

  if (hpc %in% c("array")) { # For hpc == "array", use the specific files as the starting point

    esm <- dir(indir, pattern = file, full.names = TRUE)

    if (stringr::str_to_lower(freq) == "yearly") { # run function
      change_yearly(esm, outdir)
    } else if (stringr::str_to_lower(freq) == "monthly") {
      change_monthly(esm, outdir)
    }

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

  esms <- dir(indir, pattern = "*.nc", full.names = TRUE)

  future::plan(future::multisession, workers = w)

  if (stringr::str_to_lower(freq) == "yearly") {
    furrr::future_walk(esms, change_yearly, outdir) # JDE
  } else if (stringr::str_to_lower(freq) == "monthly") {
    furrr::future_walk(esms, change_monthly, outdir) # JDE
  }

  future::plan(future::sequential)

  }
}
