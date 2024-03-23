# Extract CMIP6 bits from the name of the file
# Written by Dave Schoeman and Tin Buenafe
  # Last modified: Feb 2024

pacman::p_load(tidyverse)

get_CMIP6_bits <- function(file_name) {
  bits <- str_split(basename(file_name), "_") %>% 
    unlist()
  date_start_stop <- bits[7] %>% 
    str_split("[.]") %>%
    map(1) %>% 
    unlist() %>% 
    str_split("-") %>%
    unlist()
  if(str_detect(file_name, "_.mon_")) {
    date_start_stop <- paste0(date_start_stop, c("01", "31"))
  } # Fix dates for monthly data
  if(str_detect(file_name, "_.year_")) {
    date_start_stop <- paste0(date_start_stop, c("0101", "1231"))
  } # Fix dates for annual data
  date_start_stop <- as.Date(date_start_stop, format = "%Y%m%d")
  output <- list(Variable = bits[1],
                 Frequency = bits[2],
                 Model = bits[3],
                 Scenario = bits[4],
                 Variant = bits[5],
                 Grid = bits[6],
                 Year_start = date_start_stop[1],
                 Year_end = date_start_stop[2])
  return(output)
  # e.g., map_df(dir(folder), get_CMIP6_bits)
}