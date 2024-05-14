#' Download ESM data
#'
#' pacman::p_load(furrr, parallel)
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @param indir
#' @param outdir
#'
#' @return
#' @export
#'
#' @examples
htr_download_ESM <- function(indir, # where wget files are located
                         outdir # where .nc files should be downloaded
) {

  pth <- getwd()
  w <- parallel::detectCores()-2

  files <- dir(indir, pattern = "wget", full.names = TRUE)

  future::plan(future::multisession, workers = w)
  future::future_walk(files, wget_files)
  future::plan(future::sequential)

}

#' wget files
#'
#' @param script
#'
#' @return
#'
#' @noRd
#'
wget_files <- function(script) {
  setwd(outdir)
  system(paste0("bash ", script, " -s")) # Change the path to where you want the data stored, then run wget from there
  setwd(pth) # change back the working directory
}