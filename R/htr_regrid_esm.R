#' Regrid the ESMs
#'
#' @author David Schoeman and Tin Buenafe
#'
#' @inheritParams htr_slice_period
#' @param cell_res Resolution to which the ESM will be regridded
#' @param layer The layer to be regridded
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' htr_regrid_esm(
#' indir = file.path(base_dir, "data", "proc", "yearly", "tos"),
#' outdir = file.path(base_dir, "data", "proc", "regridded", "yearly", "tos"),
#' cell_res = 0.25,
#' layer = "annual"
#' )
#' }
htr_regrid_esm <- function(indir, # input directory
                           outdir, # folder to save the regridded ESM
                           cell_res = 0.25, # resolution of blank raster
                           layer # which layer is being regridded (anomalies, annual, etc.?)
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  w <- parallel::detectCores() - 2

  base_rast <- htr_make_blankRaster(
    outdir,
    cell_res
  )

  # Get files and remap
  netCDFs <- dir(indir, full.names = TRUE)

  ##############

  remap_netCDF <- function(anom_file, base_rast, layer) {
    new_name <- basename(anom_file) %>%
      stringr::str_replace(layer, paste0("Regridded", stringr::str_to_sentence(layer)))

    # Standard, terra-compatible cell-res degree grid
    bits <- htr_get_CMIP6_bits(basename(anom_file))

    print(paste(bits$Model, bits$Scenario))

    out_file <- anom_file %>%
      stringr::str_replace(indir, outdir) %>%
      stringr::str_replace(basename(anom_file), new_name)

    if (bits$Variable == "pr") { # For precipitation, use conservative remapping
      cdo_code <- paste0("cdo -s -L -remapcon,", base_rast, " ", anom_file, " ", out_file)
      system(cdo_code)
    } else { # For everything else, use bilinear interpolation, although Bio-ORACLE uses remapdis, so consider changing to that
      cdo_code <- paste0("cdo -s -L -remapbil,", base_rast, " ", anom_file, " ", out_file)
      system(cdo_code)
    }
  }

  ##############

  future::plan(future::multisession, workers = w)
  furrr::future_walk(netCDFs, remap_netCDF, base_rast, layer)
  future::plan(future::sequential)

  system(paste0("rm -r ", base_rast))
}
