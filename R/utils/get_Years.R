# Get data from range of years --------------------------------------------------

pacman::p_load(lubridate, tidyverse)

get_Years <- function(nc_file, yr1, yr2, infold, outfold, overwrite) {
  bits <- get_CMIP6_bits(nc_file)
  y1 <- year(bits$Year_start)	
  y2 <- year(bits$Year_end)
  if((y1 < yr1 | y2 > yr2) || isFALSE(overwrite)) {
    new_name <- nc_file %>% 
      str_split(paste0("_", as.character(y1))) %>%
      map(1) %>% 
      unlist() %>%
      paste0(., "_", yr1, "0101-", yr2, "1231.nc")
    system(paste0("cdo selyear,", yr1, "/", yr2, " ", infold, "/", nc_file, " ", outfold, "/", new_name))
    # file.remove(paste0(infold, "/", nc_file))
  } else {
    cat("Nothing to do!")
    cat("\n")
  }
}