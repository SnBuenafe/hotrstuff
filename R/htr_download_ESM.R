#' Download Earth System Model (ESM) data using wget scripts
#'
#' This function downloads climate model data from remote repositories using wget
#' scripts. It processes multiple wget scripts in parallel to efficiently download
#' large climate datasets, typically from CMIP6 data nodes or similar repositories.
#'
#' @details
#' The function executes bash wget scripts that contain download commands for climate
#' data files. It changes the working directory to the output directory before running
#' each wget script to ensure files are downloaded to the correct location.
#'
#' The process involves:
#' 1. Finding all wget script files in the input directory
#' 2. For each script, changing to the output directory
#' 3. Executing the wget script with the `-q` flag (quiet mode)
#' 4. Restoring the original working directory
#'
#' All wget scripts are processed in parallel using multiple workers for efficient
#' downloading of large datasets.
#'
#' @author Dave Schoeman and Tin Buenafe
#'
#' @inheritParams htr_slice_period
#' @param indir Character string. Directory containing wget script files. These are
#'   typically bash scripts with wget commands for downloading climate data from
#'   remote repositories (e.g., ESGF data nodes).
#' @param outdir Character string. Directory where the downloaded NetCDF files will
#'   be saved. The function will change to this directory before executing wget scripts.
#' @param quiet Logical. If `TRUE` (default), the download progress is shown.
#'   If `FALSE`, the download progress is not shown.
#' @param security Logical. If `FALSE` (default), skips security checks.
#' Note that this option will only work if the data is not secured at all. If `TRUE`,
#' user must input a character string in either the `openid` argument or
#' in the `certificate` argument.
#' @param openid Character string. String of the OpenID that can be used
#' to download secure files.
#' @param certificate Character string. String of the certificate that can be used
#' to download secure files.
#'
#' @return
#' No return value. The function downloads NetCDF files to the specified output
#' directory as defined by the wget scripts.
#'
#' @note
#' - Requires `wget` to be installed and accessible from the system PATH
#' - Wget scripts should be properly formatted bash scripts with appropriate download commands
#' - The function temporarily changes working directory during execution
#' - Uses parallel processing with (number of CPU cores - 2) workers
#' - Ensure sufficient disk space is available for downloaded climate data
#' - Network connectivity and access permissions to data repositories are required
#'
#' @references
#' ESGF Data Portal: https://esgf-node.llnl.gov/projects/esgf-llnl/
#' CMIP6 Data Access: https://pcmdi.llnl.gov/CMIP6/
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_download_ESM(
#'   indir = file.path(base_dir, "data", "raw", "wget"), # input directory
#'   outdir = file.path(base_dir, "data", "raw", "tos") # output directory
#' )
#' }
htr_download_ESM <- function(indir, # where wget files are located
                             outdir, # where .nc files should be downloaded
                             quiet = TRUE,
                             security = FALSE,
                             openid = NULL,
                             certificate = NULL,
                             ncores = NULL, # Use all available. Ignored on HPC
                             hpc = NULL # if run in the HPC, possible values are "array", "parallel"
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  pth <- getwd()

  # Define workers
  w <- htr_workers(ncores, hpc)

  ##############

  wget_files <- function(script) {
    setwd(outdir)

    if(isTRUE(quiet)) {
      system_code <- paste0("bash ", script, " -q")
    } else {
      system_code <- paste0("bash ", script)
    }

    if(isFALSE(security)) {
      system_code <- paste0(system_code, " -s")
    } else if(isTRUE(security) && length(openid) > 0) {
      system_code <- paste0(system_code, " -o ", openid)
    } else if(isTRUE(security) && length(certificate) > 0) {
      system_code <- paste0(system_code, " -c", certificate)
    } else {
      cat("You need to input your openid or a certificate to download a secure file.")
    }

    system(system_code)

    setwd(pth) # change back the working directory
  }

  ##############

  files <- htr_list_files(indir, pattern = "wget")
  if (is.null(files)) return(invisible(NULL))

  future::plan(future::multisession, workers = w)
  furrr::future_walk(files, wget_files)
  future::plan(future::sequential)
}
