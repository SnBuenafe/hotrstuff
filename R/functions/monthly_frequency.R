# Change to monthly frequency
# Written by Tin Buenafe
# Last modified: March 2024

monthly_frequency <- function(indir,
                             outdir) {
  
  w <- detectCores()-2
  
  change_monthly <- function(f) {
    
    out_file <- f %>% 
      basename() %>% 
      str_split("_merged_") %>% 
      map(~paste0(.x[1], "_monthly_", .x[2])) %>% 
      paste0(outdir, "/", .)
    cdo_code <- paste0("cdo -monmean", " ", f, " ", out_file)
    system(cdo_code)
    
  }
  
  esms <- dir(indir, pattern = "*.nc", full.names = TRUE)
  plan(multisession, workers = w)
  future_walk(esms, change_monthly)
  plan(sequential)
  
}