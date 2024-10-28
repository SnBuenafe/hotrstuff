#' Print depth-resolved levels
#'
#' @inheritParams htr_slice_period
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' htr_show_levels(
#' indir = file.path(base_dir, "data", "proc", "sliced", "omip", variable)
#' )
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
