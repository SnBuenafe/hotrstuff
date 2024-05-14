#' Regrid the ESMs
#'
#' pacman::p_load(parallel, tidyverse, furrr)
#'
#' @author David Schoeman and Tin Buenafe
#'
#' @param indir
#' @param outdir
#' @param blankrast_dir
#' @param cell_res
#' @param layer
#'
#' @return
#' @export
#'
#' @examples
htr_regrid_esm <- function(indir, # input directory
                       outdir, # folder to save the regridded ESM
                       blankrast_dir, # where the blank raster would be saved
                       cell_res = 0.25, # resolution of blank raster
                       layer # which layer is being regridded (anomalies, annual, etc.?)
) {

  w <- parallel::detectCores()-2

  base_rast <- make_blankRaster(blankrast_dir,
                                cell_res)

  # Get files and remap
  netCDFs <- dir(indir, full.names = TRUE)
  future::plan(future::multisession, workers = w)
  future::future_walk(netCDFs, remap_netCDF, base_rast)
  future::plan(future::sequential)
  system(paste0("rm -r ", blankrast_dir))

}


#' Remap Netcdf
#'
#' @param anom_file
#'
#' @return
#'
#' @noRd
remap_netCDF <- function(anom_file, base_rast) {
  new_name <- basename(anom_file) %>%
    stringr::str_replace(layer, paste0("Regridded", stringr::str_to_sentence(layer)))

  # Standard, terra-compatible cell-res degree grid
  bits <- get_CMIP6_bits(basename(anom_file))

  print(paste(bits$Model, bits$Scenario))

  out_file <- anom_file %>%
    stringr::str_replace(indir, outdir) %>%
    stringr::str_replace(basename(anom_file), new_name)

  if(bits$Variable == "pr") { # For precipitation, use conservative remapping
    cdo_code <- paste0("cdo -s -L -remapcon,", base_rast, " ", anom_file, " ", out_file)
    system(cdo_code)
  } else { # For everything else, use bilinear interpolation, although Bio-ORACLE uses remapdis, so consider changing to that
    cdo_code <- paste0("cdo -s -L -remapbil,", base_rast, " ", anom_file, " ", out_file)
    system(cdo_code)
  }
}
