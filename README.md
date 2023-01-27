
<!-- README.md is generated from README.Rmd. Please edit that file -->

# evsim <a href='https://mcanigueral.github.io/evsim/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/dplyr)](https://cran.r-project.org/package=dplyr) -->
<!-- [![R-CMD-check](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/tidyverse/dplyr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/dplyr?branch=main) -->
<!-- badges: end -->

## Overview

{evsim} is part of a suite of packages to analyse, model and simulate
the charging behavior of electric vehicle users:

- [{evprof}](https://mcanigueral.github.io/evprof/): Electric Vehicle
  PROFiling
- [{evsim}](https://mcanigueral.github.io/evsim/): Electric Vehicle
  SIMulation

{evsim} package provides the functions for:

- Simulating new EV sessions based on Gaussian Mixture Models created
  with package {evprof}
- Calculating the power demand from a data set of EV sessions in a
  specific time resolution
- Calculating the occupancy (number of vehicles connected) in a specific
  time resolution
- Including the EV model inputs in a Shiny Dashboard (a Shiny module is
  provided)

## Usage

If you have your own data set of EV charging sessions or you have
already built your EV model with {evprof}, the best place to start is
the [Get started
chapter](https://mcanigueral.github.io/evsim/articles/evsim.html) in the
package website.

## Installation

Since the package is not yet in CRAN, you can install the development
version of {evsim} from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("mcanigueral/evsim")
```

## Getting help

If you encounter a clear bug, please open an issue with a minimal
reproducible example on
[GitHub](https://github.com/mcanigueral/evsim/issues). For questions and
other discussion, please send me a mail to <marc.canigueral@udg.edu>.

------------------------------------------------------------------------
