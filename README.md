
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hotrstuff <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Ubuntu](https://github.com/SnBuenafe/hotrstuff/actions/workflows/Ubuntu.yaml/badge.svg)](https://github.com/SnBuenafe/hotrstuff/actions/workflows/Ubuntu.yaml)
[![MacOS](https://github.com/SnBuenafe/hotrstuff/actions/workflows/MacOS.yaml/badge.svg)](https://github.com/SnBuenafe/hotrstuff/actions/workflows/MacOS.yaml)
[![Windows](https://github.com/SnBuenafe/hotrstuff/actions/workflows/Windows.yaml/badge.svg)](https://github.com/SnBuenafe/hotrstuff/actions/workflows/Windows.yaml)
[![Codecov test
coverage](https://github.com/SnBuenafe/hotrstuff/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/SnBuenafe/hotrstuff/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The goal of hotrstuff is to …

## Requirements

hotrstuff requires cdo to be installed. More about cdo installation can
be found on the [cdo installer
website](https://code.mpimet.mpg.de/projects/cdo/wiki#Installation-and-Supported-Platforms)

### Mac

The easiest way to install `cdo` is using Homebrew:

    brew install cdo

More information can be found on the [CDO Mac
Website](https://code.mpimet.mpg.de/projects/cdo/wiki/MacOS_Platform).

### Windows

Recent versions of Windows (\>=10) included an Ubuntu embedded Linux.
This environment offers a clean integration with the windows file
systems and and the opportunity to install CDO via the native package
manager of Ubuntu:

    sudo apt-get upgrade
    sudo apt-get install cdo

More information can be found on the [CDO Windows
Website](https://code.mpimet.mpg.de/projects/cdo/wiki/Win32)

### Unix/Linux

    sudo apt-get upgrade
    sudo apt-get install cdo

## Installation

You can install the development version of hotrstuff from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SnBuenafe/hotrstuff")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hotrstuff)
## basic example code
```

## Citation
