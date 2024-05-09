# Create a blank raster
# Written by David Schoeman and Tin Buenafe
  # Last modified: March 2024

pacman::p_load(terra)

make_blankRaster <- function(blankrast_dir, # directory to save the blank raster
                             cell_res # resolution of the cell
) {
  
  base_rast <- paste0(blankrast_dir, "/base_rast.nc")
  r <- rast(resolution = cell_res)
  r[] <- 1
  mask2netCDF4(r, pth = blankrast_dir, 
               ncName = basename(base_rast), 
               dname = "dummy", 
               dlname = "dummy") # changes the base_rast to netcdf4
  
  return(base_rast)
}



