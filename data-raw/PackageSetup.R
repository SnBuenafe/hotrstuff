library(devtools)
library(usethis)

create_package("~/GitHub/hotrstuff")

use_git()

use_gpl3_license()

use_testthat()

use_github()

use_readme_rmd()

use_vignette(name = "hotrstuff")

use_pipe()

# CMD-OPT-SHIFT-R will create the Roxygen header
use_r("utils")
use_r("htr_calc_anomalies.R") # Split functions. Don't export the inner ones
use_r("htr_calc_mean.R")
use_r("htr_create_ensemble.R")
use_r("htr_download_ESM.R")
use_r("htr_fix_calendar.R")
use_r("htr_merge_files.R")
use_r("htr_regrid_esm.R")
use_r("htr_slice_period.R")
use_r("htr_change_freq.R") # Merge monthly/yearly freq

devtools::check()

use_package("parallel")
use_package("ncdf4")
use_package("lubridate")
use_package("terra")
use_package("stringr")
use_package("future")
use_package("purrr")
use_package("dplyr")

devtools::build_readme()

# Setup a data-raw folder to use for data creation and storage
use_data_raw()


## Setup testing
use_test("utils")
use_test("htr_calc_anomalies.R")
use_test("htr_calc_mean.R")
use_test("htr_create_ensemble.R")
use_test("htr_download_ESM.R")
use_test("htr_fix_calendar.R")
use_test("htr_merge_files.R")
use_test("htr_regrid_esm.R")
use_test("htr_slice_period.R")
use_test("htr_change_freq.R")


# Style the package and ensure the code can breathe :-)
styler::style_pkg()


use_lifecycle_badge(stage = "experimental")

# Github Actions
use_github_action(name = "check-standard", badge = TRUE)

usethis::use_github_action("test-coverage", badge = TRUE)

## Create hex sticker
source("data-raw/hexSticker.R")

# Build website
usethis::use_pkgdown()

devtools::build_site()

# Add logo to the correct location for the website
usethis::use_logo("data-raw/logo.png")

# Create favicons for the site
pkgdown::build_favicons(pkg = ".", overwrite = TRUE)

# Rebuild website to include favicons/logos
devtools::build_site()
