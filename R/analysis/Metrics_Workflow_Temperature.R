# Trial workflow for calculating temperature metrics
pacman::p_load(tidyverse, here, terra)

# Call utils
list <- list.files(here("src", "utils"), full.names = TRUE)
walk(list, source)

# ---- Step 9: Trim to different periods in the future ----
source(here("src", "functions", "slice_period.R"))

# Trimming the ensembles

# SSP (all SSPs)
# Near-future
make_folder(here("data", "metrics", "trimmed", "near_future", "tos"))
slice_period(indir = here("data", "proc", "ensemble", "mean", "tos", "ssp"),
             outdir = here("data", "metrics", "trimmed", "near_future", "tos"),
             frq = "Omon", # ocean, monthly
             scenario = "ssp",
             year_start = 2015,
             year_end = 2040,
             overwrite = FALSE
)

make_folder(here("data", "metrics", "trimmed", "mid_future", "tos"))
slice_period(indir = here("data", "proc", "ensemble", "mean", "tos", "ssp"),
             outdir = here("data", "metrics", "trimmed", "mid_future", "tos"),
             frq = "Omon", # ocean, monthly
             scenario = "ssp",
             year_start = 2041,
             year_end = 2070,
             overwrite = FALSE
)

make_folder(here("data", "metrics", "trimmed", "far_future", "tos"))
slice_period(indir = here("data", "proc", "ensemble", "mean", "tos", "ssp"),
             outdir = here("data", "metrics", "trimmed", "far_future", "tos"),
             frq = "Omon", # ocean, monthly
             scenario = "ssp",
             year_start = 2071,
             year_end = 2100,
             overwrite = FALSE
)


# ---- Step 10: Calculate chronic exposure metrics ----
source(here("src", "functions", "calculate_roc.R"))

# Historical
calculate_roc(indir = here("data", "proc", "ensemble", "mean", "tos", "historical"),
              outdir = here("data", "metrics", "roc", "tos"),
              year_start = 1985,
              year_end = 2014)

# SSPs
calculate_roc(indir = here("data", "metrics", "trimmed", "near_future", "tos"),
              outdir = here("data", "metrics", "roc", "tos"),
              year_start = 2015,
              year_end = 2040,
              timeframe = "near_future")

calculate_roc(indir = here("data", "metrics", "trimmed", "mid_future", "tos"),
              outdir = here("data", "metrics", "roc", "tos"),
              year_start = 2041,
              year_end = 2070,
              timeframe = "mid_future")

calculate_roc(indir = here("data", "metrics", "trimmed", "far_future", "tos"),
              outdir = here("data", "metrics", "roc", "tos"),
              year_start = 2071,
              year_end = 2100,
              timeframe = "far_future")

# ---- Step 11: Calculate climate velocity ----
# For temperature
source(here("src", "functions", "calculate_velocity.R"))

# Historical
calculate_velocity(indir = here("data", "proc", "ensemble", "mean", "tos", "historical"),
                   outdir = here("data", "metrics", "velocity", "tos"),
                   year_start = 1985,
                   year_end = 2014)

# SSPs
calculate_velocity(indir = here("data", "metrics", "trimmed", "near_future", "tos"),
              outdir = here("data", "metrics", "velocity", "tos"),
              year_start = 2015,
              year_end = 2040,
              timeframe = "near_future")

calculate_velocity(indir = here("data", "metrics", "trimmed", "mid_future", "tos"),
              outdir = here("data", "metrics", "velocity", "tos"),
              year_start = 2041,
              year_end = 2070,
              timeframe = "mid_future")

calculate_velocity(indir = here("data", "metrics", "trimmed", "far_future", "tos"),
              outdir = here("data", "metrics", "velocity", "tos"),
              year_start = 2071,
              year_end = 2100,
              timeframe = "far_future")

