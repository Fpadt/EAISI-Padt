
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Pythia’s Advice - Datascience Tooling <a href="https://www.smart-r.net" target="_blank" rel="noopener noreferrer"><img src="man/figures/padt_wtxt.png" align="right" height="138" /></a>

*Under Development*

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidyverse/ggplot2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/ggplot2/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/Fpadt/padt/branch/main/graph/badge.svg?token=8FXGGC2M8C)](https://codecov.io/gh/Fpadt/padt)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/padt)](https://cran.r-project.org/package=padt)
<!-- badges: end -->

## Overview

### PADT

Pythia’s Advice - Datascience Tooling (PADT) is a collection of tools
and functions that are used in the context of the Pythia’s Advice
project. This project is the final module representing the pinnacle of
the program [Master Data &
AI](https://www.tue.nl/en/research/institutes/eindhoven-artificial-intelligence-systems-institute/ai-education/professional-education/programs-voor-professionals/mastering-data-ai-mdai)
at
[EAISI](https://www.tue.nl/en/research/institutes/eindhoven-artificial-intelligence-systems-institute).

The project aims to provide a set of tools and functions to demand
planners to perform data analysis and time-series forecasting on data
sets in csv format which are tranformed in parquet files using [Duck
DB](https://duckdb.org/) providing lightning fast data access.

The project is developed in R and Python and is maintained by
[smart-R](https://www.smart-r.net/).

## Installation

The development version of EAISI-Padt can be installed from GitHub
using:

``` r
# not released on CRAN yet!!

# the development version from GitHub:
remotes::install_github("Fpadt/EAISI-Padt")
```

### Prerequisites

The following software needs to be installed as prerequisite (preferably
the lastest version):

1.  [The R Project for Statistical
    Computing](https://cran.r-project.org/)
2.  Config file maintained!!!, see directory `./padt_config`

## Example

This is a basic example which shows you how to access data:

``` r
library(padt)

IPM <-
  pa_td_sap_get(
    .ftype        = 6              , # 5 = rtp, 6 = ipm  
    .material     = '10023'        , # Optional user-supplied material
    .salesorg     = 'FR30'         , # Optional user-supplied salesorg
    .scope_matl   = TRUE           , # restrict to Pythia Scope
    .scope_sorg   = TRUE           , # restrict to Pythia Scope
    .cm_min       = '202101'       , # minimal Cal Month
    .cm_max       = '202506'       , # maximal Cal Month,
    .n            = Inf
  )
```
