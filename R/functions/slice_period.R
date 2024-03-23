# Trim time frame based on start and end months
# Written by Dave Schoeman and Tin Buenafe
  # Last modified: March 2024

pacman::p_load(furrr, tidyverse, parallel)

trim_period <- function(f, # file
                        scenario, # historical or ssp
                        indir,
                        outdir,
                        year_start,
                        year_end,
                        overwrite) {
  
  dt1 <- get_CMIP6_bits(f)$Year_start %>% 
    as.Date()
  
  dt2 <- get_CMIP6_bits(f)$Year_end %>% 
    as.Date()
  
  if(dt1 <= as.Date(paste0(year_start, "-01-01")) | dt2 >= as.Date(paste0(year_end, "-12-31"))) {
    get_Years(f, year_start, year_end, indir, outdir, overwrite) # replacing files in merged folder with trimmed files
    
    if(isTRUE(overwrite)) {
      terminal_code <- paste0("rm ", indir, "/", f)
      system(terminal_code)
    }
    
  }
  
}


slice_period <- function(indir, # where the merged files are
                         outdir, # where the trimmed files will be saved
                         frq, # frequency
                         scenario, # historical or ssp
                         year_start,
                         year_end,
                         overwrite # TRUE or FALSE
                         ) {
  
  w <- detectCores()-2
  
  files <- dir(indir, pattern = paste0("_", frq, "_"))
  
  files <- files[str_detect(files, scenario)]

  trim_timeframe <- function(f) {
    
    s <- get_CMIP6_bits(f)$Scenario
    m <- get_CMIP6_bits(f)$Model
    
    print(paste0(m, "_", s))
    
    if(str_detect(s, scenario)) {
      
      trim_period(f,
                  s,
                  indir,
                  outdir,
                  year_start,
                  year_end,
                  overwrite)
      
    } else {
      print(paste0("Scenario ", s, " was not chosen"))
    }
  
  }
  
  plan(multisession, workers = w)
  future_walk(files, trim_timeframe)
  plan(sequential)
  
}