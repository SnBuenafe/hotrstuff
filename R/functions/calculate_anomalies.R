# Calculate anomalies relative to the baseline mean
# Written by Dave Schoeman and Tin Buenafe
  # Last modified: March 2024

calculate_anomalies <- function(indir, # input directory of the projections
                                mndir, # directory of baseline mean
                                outdir # where anomalies will be saved
) {
  
  w <- detectCores()-2
  
  # For each model-variable combination for for which the historical mean could be computed, compute anomalies for all time periods relative to the base
  do_anom <- function(v, fr, m) {
    files <- dir(indir, full.names = TRUE) %>% 
      str_subset(paste0("(?=.*", v, "_", ")(?=.*", fr, "_", ")(?=.*", m, "_", ")")) # For each combination of variable-frequency-model that we have a baseline mean for, find merged files for all time periods 
    subtract_mean <- function(f) { # For each file found, subtract the mean and save
      bits <- basename(f) %>% 
        get_CMIP6_bits(.)
      mn <- dir(mndir, pattern = paste0(bits$Variable, "_", bits$Frequency, "_", bits$Model)) %>% 
        paste0(mndir, "/", .)
      anom_out <- str_replace_all(f, dirname(f), outdir) %>% 
        str_replace("_merged_", "_anomalies_")
      cdo_code <- paste0("cdo sub ", f, " ", mn, " ", anom_out)
      system(cdo_code)
    }
    walk(files, subtract_mean)
  }
  
  x <- get_meta(mndir, 
                string = c("Variable", "Frequency", "Model")) # get metadata from the files in the baseline directory
  plan(multisession, workers = w)
  future_pwalk(x, do_anom)  
  plan(sequential)
  
}


