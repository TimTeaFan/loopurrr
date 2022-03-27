
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loopurrr

<!-- badges: start -->

![Release
status](https://img.shields.io/badge/status-first%20release-yellow)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/TimTeaFan/loopurrr/workflows/R-CMD-check/badge.svg)](https://github.com/TimTeaFan/loopurrr/actions)
[![Codecov test
coverage](https://codecov.io/gh/TimTeaFan/loopurrr/branch/main/graph/badge.svg)](https://codecov.io/gh/TimTeaFan/loopurrr?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/timteafan/dplyover/badge)](https://www.codefactor.io/repository/github/timteafan/loopurrr)
[![CRAN
status](https://www.r-pkg.org/badges/version/dplyover)](https://cran.r-project.org/package=dplyover)
<!-- badges: end -->

## Overview

{loopover} is a extends {dplyr}’s functionality by building a function
family around `dplyr::across()`.

The goal of this *over-across function family* is to provide a concise
and uniform syntax which can be used to create columns by applying
functions to vectors and/or sets of columns in {dplyr}. Ideally, this
will:

-   **reduce the amount of code** to create variables derived from
    existing colums, which is especially helpful when doing exploratory
    data analysis (e.g. lagging, collapsing, recoding etc. many
    variables in a similar way).
-   **provide a clean {dplyr} approach** to create many variables which
    are calculated based on two or more variables.
-   **improve our mental model** so that it is easier to tackle problems
    where the solution is based on creating new columns.

The functions in the *over-apply function family* create columns by
applying one or several functions to:

## Installation

{loopurrr} is not on CRAN yet. You can install the latest version from
[GitHub](https://github.com/TimTeaFan/loopurrr) with:

``` r
# install.packages("remotes")
remotes::install_github("TimTeaFan/loopurrr")
```

## Getting started

…

``` r
library(loopurrr)
## basic example code
```

…

## Supported functions

## History

## Acknowledgements and Disclaimer
