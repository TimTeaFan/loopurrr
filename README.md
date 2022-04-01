
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loopurrr

<!-- badges: start -->

![Release
status](https://img.shields.io/badge/status-under%20construction-red)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/TimTeaFan/loopurrr/workflows/R-CMD-check/badge.svg)](https://github.com/TimTeaFan/loopurrr/actions)
[![Codecov test
coverage](https://codecov.io/gh/TimTeaFan/loopurrr/branch/main/graph/badge.svg)](https://codecov.io/gh/TimTeaFan/loopurrr?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/timteafan/loopurrr/badge)](https://www.codefactor.io/repository/github/timteafan/loopurrr)
[![CRAN
status](https://www.r-pkg.org/badges/version/loopurrr)](https://cran.r-project.org/package=loopurrr)
<!-- badges: end -->

## Overview

<p id="logop">
<a id="logo" href="https://raw.githubusercontent.com/TimTeaFan/loopurrr/main/man/figures/logo_big.png"><img src="https://raw.githubusercontent.com/TimTeaFan/loopurrr/main/man/figures/logo.png" alt="loopurrr's logo a cat sleeping in the form of a circle" align="right"></a>
</p>

{loopurrr} makes {purrr}’s iterator functions more understandable and
easier to debug.

In this initial version, {loopurrr} consists only of *one* main
function: `as_loop()`.

`as_loop()` takes a function call to one of {purrr}’s iterator
functions, such as `purrr::map()`, and translates it into a regular
`for` loop.

You might ask: *“Why would anyone want to do this?!”* 😲😮

`as_loop()` has at least three use cases: 😀😀

#### 1. Learning and Teaching Functional Programming

Beginners, and especially users new to functional-style programming
often have a hard time getting their head around R’s rather opaque
iterator functions, such as base R’s `lapply()` or `purrr::map()`. `for`
loops, on the other hand, are fairly well understood, even by users new
to R.

`as_loop()` translates a function call to one of {purrr}’s iterator
functions into a regular `for` loop. Through this `as_loop()` shows how
{purrr}’s iterator functions work under the hood. After reading about
what iterator functions do (for example
[here](https://r4ds.had.co.nz/iteration.html#the-map-functions)),
LearneRs can start playing around with calling `as_loop()` on the
examples in the {purrr} documentation. TeacheRs, on the other hand, can
use `as_loop()` interactively when introducing the different types of
iterator functions in the {purrr} package.

#### 2. Debugging

Once learneRs know what an iterator function does and how to use it, the
next hurdle to take is dealing with failure. Iterator functions
introduce an additional layer of complexity, because special knowledge
is required to debug non-running code. By translating an iterator
function into a regular `for` loop, `as_loop()` can help debug
functions. When a `for` loop is executed in the global environment it
will run over an index, for example `i`. Once the code throws an error,
useRs can easily inspect the code at index `i` without any special
knowledge of how to use a debugger, `browser()` or `purrr::safely()`. Of
course, useRs are highly encouraged to learn how to use R’s and
{purrr}’s debugging tools and functions. However, in data science teams
with different levels of programming knowledge, the possibility to
translate complex iterator functions to regular `for` loops can help
mitigate knowledge gaps.

#### 3. Accessing and Extending {purrr} Functions

Dependent free

Extendable

## Installation

{loopurrr} is not on CRAN yet. You can install the latest version from
[GitHub](https://github.com/TimTeaFan/loopurrr) with:

``` r
# install.packages("remotes")
remotes::install_github("TimTeaFan/loopurrr")
```

## Getting started

…

First, lets use `get_supported_fns("as_loop")` to get a glimpse of what
iterator functions from the {purrr} package are currently supported by
`as_loop()`:

    #> $map
    #>  [1] "map"     "map_at"  "map_chr" "map_dbl" "map_df"  "map_dfc" "map_dfr"
    #>  [8] "map_if"  "map_int" "map_lgl" "map_raw"
    #> 
    #> $imap
    #> [1] "imap"     "imap_chr" "imap_dbl" "imap_dfc" "imap_dfr" "imap_int" "imap_lgl"
    #> [8] "imap_raw"
    #> 
    #> $map
    #> [1] "map2"     "map2_chr" "map2_dbl" "map2_df"  "map2_dfc" "map2_dfr" "map2_int"
    #> [8] "map2_lgl" "map2_raw"
    #> 
    #> $pmap
    #> [1] "pmap"     "pmap_chr" "pmap_dbl" "pmap_df"  "pmap_dfc" "pmap_dfr" "pmap_int"
    #> [8] "pmap_lgl" "pmap_raw"
    #> 
    #> $lmap
    #> [1] "lmap"    "lmap_at"
    #> 
    #> $modify
    #> [1] "modify"    "modify_at" "modify_if" "modify2"   "imodify"  
    #> 
    #> $walk
    #> [1] "iwalk" "pwalk" "walk"  "walk2"
    #> 
    #> $accumulate
    #> [1] "accumulate"  "accumulate2"
    #> 
    #> $reduce
    #> [1] "reduce"  "reduce2"

## Roadmap and Collaboration

For future versions of {loopurrr} the following milestones are planned:

-   release {loopurrr} on CRAN
-   support even more iterator functions from {purrr} (e.g. `cross`
    etc.)
-   support base R’s `apply` family in `as_loop()`
-   translate {purrr}’s iterators to base R equivalents with `as_base()`

If anyone is interested in collaborating on one or more of those
milestones, any help is appreciated! Feel free to reach out to me.

## History

## Acknowledgements and Disclaimer
