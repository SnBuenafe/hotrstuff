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
#' indir = file.path(base_dir, "data", "raw", "wget"), # input directory
#' outdir = file.path(base_dir, "data", "raw", "tos") # output directory
#' )
#' }
htr_download_ESM <- function(indir, # where wget files are located
                             outdir) { # where .nc files should be downloaded

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  pth <- getwd()

  w <- parallel::detectCores() - 2

  files <- dir(indir, pattern = "wget", full.names = TRUE)

  ##############

  wget_files <- function(script) {
    setwd(outdir)
    system(paste0("bash ", script, " -s")) # Change the path to where you want the data stored, then run wget from there
    setwd(pth) # change back the working directory
  }

  ##############

  future::plan(future::multisession, workers = w)
  furrr::future_walk(files, wget_files)
  future::plan(future::sequential)
}
