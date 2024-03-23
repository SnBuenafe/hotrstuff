# Regrid the ESMs
# Written by David Schoeman and Tin Buenafe
  # Last modified: March 2024

pacman::p_load(parallel, tidyverse, furrr)

regrid_esm <- function(indir, # input directory
                       outdir, # folder to save the regridded ESM
                       blankrast_dir, # where the blank raster would be saved
                       cell_res = 0.25, # resolution of blank raster
                       layer # which layer is being regridded (anomalies, annual, etc.?)
                       ) {
  
  w <- detectCores()-2
  
  base_rast <- make_blankRaster(blankrast_dir,
                                cell_res)
  
  remap_netCDF <- function(anom_file) {
    new_name <- basename(anom_file) %>% 
      str_replace(layer, paste0("Regridded", str_to_sentence(layer)))
    
    # Standard, terra-compatible cell-res degree grid
    bits <- get_CMIP6_bits(basename(anom_file))
    
    print(paste(bits$Model, bits$Scenario))
    
    out_file <- anom_file %>% 
      str_replace(indir, outdir) %>% 
      str_replace(basename(anom_file), new_name)
    
    if(bits$Variable == "pr") { # For precipitation, use conservative remapping
      cdo_code <- paste0("cdo -s -L -remapcon,", base_rast, " ", anom_file, " ", out_file)
      system(cdo_code)
    } else { # For everything else, use bilinear interpolation, although Bio-ORACLE uses remapdis, so consider changing to that
      cdo_code <- paste0("cdo -s -L -remapbil,", base_rast, " ", anom_file, " ", out_file)
      system(cdo_code)
    }
  }
 
  # Get files and remap
  netCDFs <- dir(indir, full.names = TRUE)
  plan(multisession, workers = w)
  future_walk(netCDFs, remap_netCDF)
  plan(sequential)
  system(paste0("rm -r ", blankrast_dir)) 
  
}