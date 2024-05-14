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
  w <- detectCores()-2

  files <- dir(indir, pattern = "wget", full.names = TRUE)

  plan(multisession, workers = w)
  future_walk(files, wget_files)
  plan(sequential)

}

#' wget files
#'
#' @param script
#'
#' @return
#' @export
#'
#' @examples
wget_files <- function(script) {
  setwd(outdir)
  system(paste0("bash ", script, " -s")) # Change the path to where you want the data stored, then run wget from there
  setwd(pth) # change back the working directory
}
