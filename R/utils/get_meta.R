# Get combinations of variables, frequency, experiments/scenarios, models, and variants from the netCDF files
# Written by Dave Schoeman and Tin Buenafe
  # Last modified: Feb 2024

pacman::p_load(purrr, tidyverse)

get_meta <- function(x, 
                     string # refers to the aspects extracted per climate model
                     ) {
  
  y <- dir(x) %>% 
    map(get_CMIP6_bits) %>% 
    map(`[`, string) %>% 
    map(bind_cols) %>% 
    bind_rows() %>% 
    distinct() %>% 
    as.list() %>% 
    unname()
  
  return(y)
  
}