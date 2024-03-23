# Calculate mean of specified time period. Used to calculate baseline means.
# Written by Dave Schoeman and Tin Buenafe
  # Last modified: March 2024

calculate_mean <- function(indir, # where inputs are
                           outdir, # where outputs will be saved
                           scenario, # historical or ssp (use historical for calculating baseline means)
                           year_start, # start year for calculating mean of time period
                           year_end # end year for calculating mean of time period
                           ) {
  
  w <- detectCores()-2
  
  get_mean <- function(f) {
    out_file <- f %>% 
      basename() %>% 
      str_split("_merged_") %>% 
      map(~paste0(.x[1], "_mean_", year_start, "0101-", year_end, "1231.nc")) %>% 
      paste0(outdir, "/", .)
    cdo_code <- paste0("cdo -L -timmean -selyear,", year_start, "/", year_end, " ", f, " ", out_file)
    system(cdo_code)
  }
  
  esms <- dir(indir, pattern = scenario, full.names = TRUE)
  plan(multisession, workers = w)
  future_walk(esms, get_mean)
  plan(sequential)
  
}