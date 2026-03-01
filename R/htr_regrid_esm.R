#' Regrid Earth System Model outputs to a common spatial grid
#'
#' This function regrids climate model outputs from their native grids to a common
#' regular latitude-longitude grid using CDO (Climate Data Operators). This is
#' essential for comparing and combining data from different climate models that
#' use different spatial grids.
#'
#' @details
#' Different climate models use various spatial grids (regular lat-lon, curvilinear,
#' unstructured, etc.), making direct comparison difficult. This function standardizes
#' all data to a regular latitude-longitude grid using CDO interpolation methods.
#'
#' The function uses different CDO remapping operators based on the variable type:
#' - **Precipitation (`pr`)**: Uses conservative remapping (`remapcon`) to preserve
#'   total precipitation amounts
#' - **Other variables**: Uses bilinear interpolation (`remapbil`) for smooth interpolation
#'
#' The process:
#' 1. Creates a blank raster template at the specified resolution
#' 2. For each input file, determines the appropriate remapping method
#' 3. Applies CDO remapping: `cdo -s -L -remapXXX,template input output`
#' 4. Updates filenames to include "Regridded" prefix
#' 5. Cleans up the temporary template file
#'
#' The `-s` flag suppresses CDO messages, and `-L` enables netCDF4 compression.
#'
#' @author David Schoeman and Tin Buenafe
#'
#' @inheritParams htr_slice_period
#' @param cell_res Numeric. Spatial resolution in degrees for the target grid
#'   (e.g., 0.25 for quarter-degree resolution, 1.0 for one-degree resolution).
#'   Default is 0.25 degrees.
#' @param layer Character string. Description of the data layer being regridded
#'   (e.g., "annual", "monthly", "anomalies"). This is used for filename generation
#'   and progress reporting.
#' @param overwrite Logical. If `FALSE` (default), skips files that already exist
#'   in the output directory. If `TRUE`, regenerates files even if they exist.
#'
#' @return
#' No return value. The function creates regridded files in the specified output
#' directory with "Regridded" added to the layer name in the filename
#' (e.g., "_annual_" becomes "_RegriddedAnnual_").
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Creates a temporary grid template file that is automatically cleaned up
#' - Uses conservative remapping for precipitation to preserve mass conservation
#' - Uses bilinear interpolation for other variables (consider `remapdis` for some applications)
#' - Progress messages show the model and scenario being processed
#' - Uses parallel processing when `hpc` is not set to "array"
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO remapbil operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=115
#' CDO remapcon operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=116
#' Grid remapping methods: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#section.1.3.2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' htr_regrid_esm(
#'   indir = file.path(base_dir, "data", "proc", "yearly", "tos"),
#'   outdir = file.path(base_dir, "data", "proc", "regridded", "yearly", "tos"),
#'   cell_res = 0.25,
#'   layer = "annual"
#' )
#' }
htr_regrid_esm <- function(indir, # input directory
                           outdir, # folder to save the regridded ESM
                           cell_res = 0.25, # resolution of blank raster
                           layer, # which layer is being regridded (anomalies, annual, etc.?)
                           overwrite = FALSE, # if TRUE, overwrite existing files
                           ncores = NULL, # Use all available. Ignored on HPC
                           hpc = NULL, # if run in the HPC, possible values are "array", "parallel"
                           file = NULL, # hpc = "array", the input will be the file
                           cdo_flags = "-f nc4c -z zip_1"
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Define workers
  w <- htr_workers(ncores, hpc)

  base_rast <- htr_make_blankRaster(
    outdir,
    cell_res
  )

  ##############

  remap_netCDF <- function(anom_file, base_rast, layer, overwrite) {
    new_name <- basename(anom_file) %>%
      stringr::str_replace(layer, paste0("Regridded", stringr::str_to_sentence(layer)))

    # Standard, terra-compatible cell-res degree grid
    bits <- htr_get_CMIP6_bits(basename(anom_file))

    out_file <- anom_file %>%
      stringr::str_replace(indir, outdir) %>%
      stringr::str_replace(basename(anom_file), new_name)

    if (bits$Variable == "pr") { # For precipitation, use conservative remapping
      cdo_code <- paste0("cdo ", cdo_flags, " -s -L -remapcon,", base_rast, " ", anom_file, " ", out_file)
    } else { # For everything else, use bilinear interpolation, although Bio-ORACLE uses remapdis, so consider changing to that
      cdo_code <- paste0("cdo ", cdo_flags, " -s -L -remapbil,", base_rast, " ", anom_file, " ", out_file)
    }
    htr_run_cdo(cdo_code, out_file, overwrite)
  }

  ##############

   if (isTRUE(hpc %in% "array")) { # For hpc == "array", use the specific files as the starting point

    netCDF <- htr_list_files(indir, pattern = file)
    if (is.null(netCDF)) return(invisible(NULL))

    remap_netCDF(netCDF, base_rast, layer, overwrite) # run function

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

    netCDFs <- htr_list_files(indir)
    if (is.null(netCDFs)) return(invisible(NULL))

    future::plan(future::multisession, workers = w)
    furrr::future_walk(netCDFs, remap_netCDF, base_rast, layer, overwrite)
    future::plan(future::sequential)

  }

  system(paste0("rm -r ", base_rast))

}
