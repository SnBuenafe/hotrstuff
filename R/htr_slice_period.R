#' Slice Period
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @param indir Directory where input files are located
#' @param outdir Directory where output files will be saved
#' @param freq The temporal frequency to be used in the analysis
#' @param scenario The CMIP scenario to be used in the analysis.
#' @param year_start Starting year
#' @param year_end Ending year
#' @param overwrite Should the output files be overwritten if they already exist (defaults to TRUE)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_slice_period(
#'   indir = file.path(base_dir, "data", "proc", "merged", "tos"), # input directory
#' outdir = file.path(base_dir, "data", "proc", "sliced", "tos"), # output directory
#' freq = "Omon", # ocean, daily
#' scenario = "ssp",
#' year_start = 2020,
#' year_end = 2100,
#' overwrite = FALSE
#' )
#' }
htr_slice_period <- function(indir, # where the merged files are
                             outdir, # where the trimmed files will be saved
                             freq, # frequency
                             scenario, # historical or ssp
                             year_start,
                             year_end,
                             overwrite = TRUE # TRUE or FALSE
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  w <- parallel::detectCores() - 2

  files <- dir(indir, pattern = paste0("_", freq, "_"))

  files <- files[stringr::str_detect(files, scenario)]

  trim_timeframe <- function(f) {
    s <- htr_get_CMIP6_bits(f)$Scenario
    m <- htr_get_CMIP6_bits(f)$Model

    print(paste0(m, "_", s))

    if (stringr::str_detect(s, scenario)) {
      trim_period(
        f,
        s,
        indir,
        outdir,
        year_start,
        year_end,
        overwrite
      )
    } else {
      print(paste0("Scenario ", s, " was not chosen"))
    }
  }

  future::plan(future::multisession, workers = w)
  furrr::future_walk(files, trim_timeframe)
  future::plan(future::sequential)
}






#' Trim time frame based on start and end months
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#'
#' @noRd
trim_period <- function(f, # file
                        scenario, # historical or ssp
                        indir,
                        outdir,
                        year_start,
                        year_end,
                        overwrite) {
  dt1 <- htr_get_CMIP6_bits(f)$Year_start %>%
    as.Date()

  dt2 <- htr_get_CMIP6_bits(f)$Year_end %>%
    as.Date()

  if (dt1 <= as.Date(paste0(year_start, "-01-01")) | dt2 >= as.Date(paste0(year_end, "-12-31"))) {
    htr_get_Years(f, year_start, year_end, indir, outdir, overwrite) # replacing files in merged folder with trimmed files

    if (isTRUE(overwrite)) {
      terminal_code <- paste0("rm ", indir, "/", f)
      system(terminal_code)
    }
  }
}
