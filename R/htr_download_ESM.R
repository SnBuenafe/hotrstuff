#' Download ESM data
#'
#' @author Dave Schoeman and Tin Buenafe
#' @inheritParams htr_slice_period
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_download_ESM(
#' hpc = NA,
#' indir = file.path(base_dir, "data", "raw", "wget"), # input directory
#' outdir = file.path(base_dir, "data", "raw", "tos") # output directory
#' )
#' }
htr_download_ESM <- function(hpc = NA, # if ran in the HPC, possible values are "array", "parallel"
                             indir, # where wget files are located
                             outdir) { # where .nc files should be downloaded

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  pth <- getwd()

  # Define workers
  if(is.na(hpc)) {
    w <- parallelly::availableCores(methods = "system", omit = 2)
  } else {
    w <- parallelly::availableCores(methods = "Slurm", omit = 2)
  }

  ##############

  wget_files <- function(script) {
    setwd(outdir)
    system(paste0("bash ", script, " -s")) # Change the path to where you want the data stored, then run wget from there
    setwd(pth) # change back the working directory
  }

  ##############

  files <- dir(indir, pattern = "wget", full.names = TRUE)

  future::plan(future::multisession, workers = w)
  furrr::future_walk(files, wget_files)
  future::plan(future::sequential)
}
