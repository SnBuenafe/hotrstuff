# Merge files according to model, variable, frequency, scenario/experiment
# Written by Dave Schoeman and Tin Buenafe
  # Last modified: Feb 2024

pacman::p_load(furrr, tidyverse, parallel)

merge_files <- function(indir, # where nc files are located
                        outdir, # where merged files should be saved
                        year_start, # start year of historical file
                        year_end # end year of projection file
) {
  
  w <- detectCores()-2
  
  l <- get_meta(indir, string = c("Variable", "Frequency", "Scenario", "Model", "Variant"))
  
  do_merge <- function(v, # variable
                       fr, # frequency
                       s, # scenario
                       m, # model
                       vt # variant
  ) {
    
    files <- dir(indir, full.names = TRUE) %>% 
      str_subset(paste0("(?=.*", v, "_", ")(?=.*", fr, "_", ")(?=.*", m, "_", ")(?=.*", s, "_", ")(?=.*", vt, "_", ")")) # in reg exp ".*" means any string of any length, so this formulation requires the variable, model and scenario to be in THAT order in a string, with each followed by "_", but with no other real constraints 
    
    # Ignore any files that don't are out of scope for our start and end years
    if(s == "historical") {
      files <- files %>% 
        basename() %>% 
        map(~get_CMIP6_bits(.x)) %>%
        map("Year_end") %>%  # Get Year_end from each element of the list
        map(~ifelse(as.Date(.x) < as.Date(paste0(year_start, "-01-01")), FALSE, TRUE)) %>% 
        unlist() %>% 
        files[.]
    } else {
      files <- files %>% 
        basename() %>% 
        map(~get_CMIP6_bits(.x)) %>%
        map("Year_start") %>%  # Get Year_start from each element of the list
        map(~ifelse(as.Date(.x) > as.Date(paste0(year_end, "-01-01")), FALSE, TRUE)) %>% 
        unlist() %>% 
        files[.]
    }
    
    if(length(files) > 0) { # Only if there are files to process
      y1 <- get_CMIP6_bits(files[1])$Year_start %>% 
        as.character() %>% 
        str_replace_all("-", "")
      y2 <- get_CMIP6_bits(files[length(files)])$Year_end %>% 
        as.character() %>% 
        str_replace_all("-", "")
      out_file <- paste0(outdir, "/", v, "_", fr, "_", m, "_", s, 
                         "_", vt, "_merged_", y1, "-", y2, ".nc")
      if(!file.exists(out_file)) {
        cdo_code <- paste0("cdo -L -selname,", "'", v , "' -mergetime ", paste0(files, collapse = " "), " ", out_file)
        system(cdo_code)
        
        print(paste0(v, "_", fr, "_", s, "_", m, "_", vt))
      }
    }
    
  }
  
  plan(multisession, workers = w) # to download wget files in parallel
  future_pwalk(l, do_merge)
  plan(sequential) # revert back to sequential processing
  
}