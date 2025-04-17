#' Merge Files
#'
#' Merge files according to model, variable, frequency, scenario/experiment
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @inheritParams htr_slice_period
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' htr_merge_files(
#'   hpc = NA,
#'   indir = file.path(base_dir, "data", "raw", "tos"), # input directory
#'   outdir = file.path(base_dir, "data", "proc", "merged", "tos"), # output directory
#'   year_start = 1985, # earliest year across all the scenarios considered
#'   year_end = 2100 # latest year across all the scenarios considered
#' )
#' }
htr_merge_files <- function(hpc = NA, # if ran in the HPC, possible values are "array", "parallel"
                            indir, # where nc files are located
                            outdir, # where merged files should be saved
                            year_start, # start year of historical file
                            year_end # end year of projection file
) {
  . <- NULL # Stop devtools::check() complaints about NSE

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Define workers
  if(is.na(hpc)) {
    w <- parallelly::availableCores(methods = "system", omit = 2)
  } else {
    w <- parallelly::availableCores(methods = "Slurm", omit = 2)
  }

  l <- htr_get_meta(indir, string = c("Variable", "Frequency", "Scenario", "Model", "Variant"))

  ##############

  do_merge <- function(v, # variable
                       fr, # frequency
                       s, # scenario
                       m, # model
                       vt # variant
  ) {
    files <- dir(indir, full.names = TRUE) %>%
      stringr::str_subset(paste0("(?=.*", v, "_", ")(?=.*", fr, "_", ")(?=.*", m, "_", ")(?=.*", s, "_", ")(?=.*", vt, "_", ")")) # in reg exp ".*" means any string of any length, so this formulation requires the variable, model and scenario to be in THAT order in a string, with each followed by "_", but with no other real constraints

    # Ignore any files that don't are out of scope for our start and end years
    if (s == "historical") {
      files <- files %>%
        basename() %>%
        purrr::map(~ htr_get_CMIP6_bits(.x)) %>%
        purrr::map("Year_end") %>% # Get Year_end from each element of the list
        purrr::map(~ ifelse(as.Date(.x) < as.Date(paste0(year_start, "-01-01")), FALSE, TRUE)) %>%
        unlist() %>%
        files[.]
    } else {
      files <- files %>%
        basename() %>%
        purrr::map(~ htr_get_CMIP6_bits(.x)) %>%
        purrr::map("Year_start") %>% # Get Year_start from each element of the list
        purrr::map(~ ifelse(as.Date(.x) > as.Date(paste0(year_end, "-01-01")), FALSE, TRUE)) %>%
        unlist() %>%
        files[.]
    }

    if (length(files) > 0) { # Only if there are files to process
      y1 <- htr_get_CMIP6_bits(files[1])$Year_start %>%
        as.character() %>%
        stringr::str_replace_all("-", "")

      y2 <- htr_get_CMIP6_bits(files[length(files)])$Year_end %>%
        as.character() %>%
        stringr::str_replace_all("-", "")

      out_file <- paste0(
        outdir, "/", v, "_", fr, "_", m, "_", s,
        "_", vt, "_merged_", y1, "-", y2, ".nc"
      )
      if (!file.exists(out_file)) {
        cdo_code <- paste0("cdo -L -selname,", "'", v, "' -mergetime ", paste0(files, collapse = " "), " ", out_file)
        system(cdo_code)

        print(paste0(v, "_", fr, "_", s, "_", m, "_", vt))
      }
    }
  }

  ##############

  future::plan(future::multisession, workers = w) # to download wget files in parallel
  furrr::future_pwalk(l, do_merge) # JDE
  future::plan(future::sequential) # revert back to sequential processing
}
