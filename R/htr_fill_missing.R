#' Replace missing values
#'
#' This function replaces missing values in the climate model files using Climate Data
#' Operators (CDO), with values depending on the method chosen. Missing values can be
#' replaced by the nearest neighbor's values, the distance-weighted average of the neighbor's
#' values, or a constant.
#'
#' @details
#' The CDO command used depends on the chosen method
#' - **Nearest neighbor**: Uses `cdo setmisstonn input output`
#' - **Distance-weighted average**: Uses `cdo setmisstodis,neighbors input output`
#' - **Constant**: Uses `cdo setmisstoc,constant input output`
#'
#' @author Tin Buenafe
#'
#' @inheritParams htr_slice_period
#' @param method Character string. Method used to calculate the missing value. Accepted methods are:
#'    - `"setmisstonn"`: Set missing values to the nearest neighbor's value
#'    - `"setmisstodis"`: Set missing values to the distance-weighted average of the neighbors.
#'    The default number of neighbors is 4, but this can be changed by changing the `neighbors` parameter.
#'    - `"setmisstoc"`: Set missing values to a defined constant (requires `constant` parameter)
#' @param neighbors Numeric. Number of neighbors used to calculate missing values. Default is 4.
#' @param constant Numeric. Constant value used to replace all missing values.
#'
#' @return
#' No return value. The function creates time-sliced files in the specified output
#' directory with the same base file names as the input.
#'
#' @note
#' - Requires CDO (Climate Data Operators) to be installed and accessible from the system PATH
#' - Uses parallel processing when `hpc` is not set to "array"
#'
#' @references
#' CDO User Guide: https://code.mpimet.mpg.de/projects/cdo/embedded/cdo.pdf
#' CDO setmiss operator: https://code.mpimet.mpg.de/projects/cdo/embedded/index.html#x1-3610002.6.15
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Fill missing values using distance-weighted average of 7 neighbors
#' htr_fill_missing(hpc = NA,
#'                 file = NA,
#'                 indir = file.path(base_dir, "data", "proc", "integrated", "tos"),
#'                 outdir = file.path(base_dir, "data", "proc", "filled", "tos"),
#'                 method = "setmisstodis",
#'                 neighbors = 7,
#'                 constant = NA
#' )
#'
#' # Fill missing values using the nearest neighbor's value
#' htr_fill_missing(hpc = NA,
#'                 file = NA,
#'                 indir = file.path(base_dir, "data", "proc", "integrated", "tos"),
#'                 outdir = file.path(base_dir, "data", "proc", "filled", "tos")
#' )
#' }
htr_fill_missing <- function(hpc = NA,
                             file = NA,
                             indir,
                             outdir,
                             method = "setmisstonn", # default is setmistonn
                             neighbors = NA, # if method = setmisstodis, default set is 4 if no number is given
                             constant = NA # if method = setmisstoc, this is required
) {

  # Create output folder if it doesn't exist
  htr_make_folder(outdir)

  # Define workers
  if(is.na(hpc)) {
    w <- parallelly::availableCores(methods = "system", omit = 2)
  } else {
    w <- parallelly::availableCores(methods = "Slurm", omit = 2)
  }

  ##############

  #TODO: Change these to assert that

  fill_missing <- function(file,
                           method,
                           constant,
                           neighbors) {

    # Naming new file
    out_file <- file %>%
      stringr::str_replace(indir, outdir)

    # Filling missing values using chosen method

    if(stringr::str_to_lower(method) %in% c("setmisstoc", "setmisstonn", "setmisstodis")) {

      method_name <- stringr::str_to_lower(method)

      if(method_name == "setmisstoc") {

        if(is.numeric(constant)) {

          system_code <- paste0("cdo ", method_name, ",", constant, " ", file, " ", out_file)

        } else {

          print("Please provide a numeric constant value.")

        }

      } else if(method_name == "setmisstonn") {

        system_code <- paste0("cdo ", method_name, ",", " ", file, " ", out_file)

      } else if(method_name == "setmisstodis") {

        if(is.numeric(neighbors)) {

          system_code <- paste0("cdo ", method_name, ",", neighbors, " ", file, " ", out_file)

        } else {

          system_code <- paste0("cdo ", method_name, " ", file, " ", out_file)

        }

      }

      system(system_code)

    } else {

      print("Please input a valid method.")

    }

  }


  ##############

  # TODO: Change this

  if (hpc %in% c("array")) { # For hpc == "array", use the specific files as the starting point

    netCDF <- dir(indir, pattern = file, full.names = TRUE)

    fill_missing(netCDF,
                 method,
                 constant,
                 neighbors) # run function

  } else { # For hpc == "parallel" and non-hpc work, use the input directory as the starting point and run jobs in parallel

    netCDFs <- dir(indir, full.names = TRUE)

    future::plan(future::multisession, workers = w)
    furrr::future_walk(netCDFs, fill_missing, method, constant, neighbors)
    future::plan(future::sequential)

  }

}
