#' Display vertical levels in depth-resolved climate data
#'
#' This function examines 3D ocean climate model data and displays the vertical
#' levels (depths or pressure levels) available in each file using CDO (Climate
#' Data Operators). This is useful for understanding the vertical structure of
#' ocean models before performing depth-related operations.
#'
#' @details
#' Ocean climate models use various vertical coordinate systems (depth in meters,
#' pressure levels, sigma coordinates, etc.). This function uses the CDO `showlevel`
#' operator to display the vertical levels present in each file, helping users
#' understand the vertical resolution and coordinate system.
#'
#' The function:
#' 1. Processes all NetCDF files in the input directory
#' 2. For each file, executes `cdo showlevel filename` to extract level information
#' 3. Prints the filename and returns the level information
#' 4. Collects all level information into a character vector
#'
#' This information is essential for:
#' - Understanding the vertical structure of ocean models
#' - Planning depth integration operations with [`htr_integrate_levels()`]
#' - Selecting appropriate level ranges for analysis
#'
#' @param indir Character string. Directory containing 3D NetCDF files with
#'   vertical levels (typically ocean model outputs with depth or pressure coordinates).
#'
#' @return
#' Character vector containing the vertical level information for each file.
#' Each element corresponds to one input file and contains the level values
#' as returned by CDO showlevel.
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Input files must be 3D data with vertical coordinate dimensions
#' - Prints filenames to console for progress tracking
#' - Level values and units depend on the model's vertical coordinate system
#' - Does not use parallel processing (processes files sequentially)
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO showlevel operator: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf#page=144
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Display levels in ocean model data
#' levels_info <- htr_show_levels(
#'   indir = file.path(base_dir, "data", "proc", "sliced", "omip", "thetao")
#' )
#'
#' # View the level information
#' print(levels_info)
#' }
htr_show_levels <- function(indir)
  {

  w <- parallel::detectCores()-2 # get number of workers

  f <- dir(indir, full.names = TRUE)

  ######

  do_show <- function(f) {

    print(basename(f))
    output <- system(paste0("cdo showlevel ", f), intern = TRUE)
    return(output)

  }

  ####

  purrr::map_vec(f, do_show)

}
