# Download ESM data
# Written by Dave Schoeman and Tin Buenafe
  # Last modified: Feb 2024

pacman::p_load(furrr, parallel)

download_ESM <- function(indir, # where wget files are located
                         outdir # where .nc files should be downloaded
                         ) {
  
  pth <- getwd()
  w <- detectCores()-2
  
  wget_files <- function(script) {
    setwd(outdir)
    system(paste0("bash ", script, " -s")) # Change the path to where you want the data stored, then run wget from there
    setwd(pth) # change back the working directory
  }
  
  files <- dir(indir, pattern = "wget", full.names = TRUE)
  
  plan(multisession, workers = w)
  future_walk(files, wget_files)
  plan(sequential)
  
}