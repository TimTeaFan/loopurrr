
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
[![CodeFactor](https://www.codefactor.io/repository/github/timteafan/loopurrr/badge)](https://www.codefactor.io/repository/github/timteafan/loopurrr)
[![CRAN
status](https://www.r-pkg.org/badges/version/loopurrr)](https://cran.r-project.org/package=loopurrr)
<!-- badges: end -->

## Overview

<p id="logop">
<a id="logo" href="https://raw.githubusercontent.com/TimTeaFan/loopurrr/main/man/figures/logo_big.png"><img src="https://raw.githubusercontent.com/TimTeaFan/loopurrr/main/man/figures/logo.png" alt="loopurrr's logo a cat sleeping in the form of a circle" align="right"></a>
</p>

`{loopurrr}` makes `{purrr}`’s iterator functions more understandable
and easier to debug. In this initial version, `{loopurrr}` consists only
of *one* main function: `as_loop()`.

`as_loop()` takes a function call to one of `{purrr}`’s iterator
functions, such as `purrr::map()`, and translates it into a regular
`for` loop.

You might ask: *“Why would anyone want to do this?!”*

`as_loop()` has at least three use cases:

1.  Learning and Teaching Functional Programming
2.  Debugging
3.  Accessing and Extending `{purrr}` Functions

The remainder of this readme will expand on the uses cases above, show
how to get started, and give a brief outlook on the development roadmap.

## Motivation and Use Cases

#### 1. Learning and Teaching Functional Programming

Beginners, and especially users new to functional-style programming,
often have a hard time getting their head around R’s rather opaque
iterator functions, such as base R’s `lapply()` or `purrr::map()`. `for`
loops, on the other hand, are fairly well understood, even by users new
to R.

`as_loop()` translates a function call to one of `{purrr}`’s iterator
functions into a regular `for` loop. Through this `as_loop()` shows how
`{purrr}`’s iterator functions work under the hood. After reading about
what iterator functions do (for example
[here](https://r4ds.had.co.nz/iteration.html#the-map-functions)),
LearneRs can start playing around with calling `as_loop()` on the
examples in the `{purrr}` documentation. TeacheRs, on the other hand,
can use `as_loop()` interactively when introducing the different types
of iterator functions in the `{purrr}` package.

Finally, this package is not only for beginners and users new to R. When
writing this package I was fairly confident in my understanding of
`{purrr}`’s iterator functions. Nevertheless, translating each of them
into a `for` loop was quite revealing, especially with the more complex
functions, such as `purrr::reduce()` (specifically when the direction is
set to `"backward"`).

#### 2. Debugging

Once learneRs know what an iterator function does and how to use it, the
next hurdle to take is dealing with failure. Iterator functions
introduce an additional layer of complexity, because special knowledge
is required to debug non-running code (see also
[here](https://r4ds.had.co.nz/iteration.html#dealing-with-failure)). By
translating an iterator function into a regular `for` loop, `as_loop()`
can help with debugging. Usually a `for` loop will run over an index,
for example `i`. When executed in the global environment, useRs can
easily inspect the code in the console at index `i` once the code throws
an error - without any special knowledge of how to use a debugger,
`browser()` or `purrr::safely()`.

Of course, useRs are still highly encouraged to learn how to use R’s and
`{purrr}`’s debugging tools and functions. However, in data science
teams with different levels of programming knowledge, the possibility to
translate complex iterator functions to regular `for` loops can help
mitigate temporary knowledge gaps.

#### 3. Accessing and Extending `{purrr}` Functions

After getting used to `{purrr}`’s functions, they easily come to mind,
when dealing with iteration problems. However, sometimes the `{purrr}`
package is not available, for example in a production environment where
new packages cannot easily be installed, or when writing a package that
doesn’t depend on `{purrr}`. Although base R equivalents exist for
`{purrr}`’s major functions, there are functions like `purrr::imap()` or
`purrr::reduce2()` which are not available in base R and need to be
constructed. In those cases, `as_loop()` provides a ready-to-use
alternative.

Further, by translating `{purrr}`’s iterator functions into `for` loops,
the underlying building blocks can easily be rearranged to create
functions that are not included in the `{purrr}` package. For example,
by translating a call to `purrr::imap()` and a call to `purrr::map2()`
we could easily build a `for` loop that loops over two vectors and an
index, as if a function like `imap2()` existed.

## Installation

`{loopurrr}` is not on CRAN yet. You can install the latest version from
[GitHub](https://github.com/TimTeaFan/loopurrr) with:

``` r
# install.packages("remotes")
remotes::install_github("TimTeaFan/loopurrr")
```

## Getting Started

First, lets use `get_supported_fns("as_loop")` to get a glimpse of which
iterator functions from the `{purrr}` package are currently supported by
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

Now we can look at the documentation of `{purrr}` and start with
translating the first example of `purrr::map()`. First, lets look at the
result:

``` r
1:3 %>% purrr::map(rnorm, n = 10)
#> [[1]]
#>  [1]  2.2774409  0.8047915 -1.1760346 -1.0427843  2.7379321  1.2038944
#>  [7]  0.7700466  2.4655117  0.5696917  1.3623644
#> 
#> [[2]]
#>  [1] 3.4517462 3.1873055 1.1531178 2.6154024 3.1894336 1.1906326 0.7301067
#>  [8] 1.4232494 0.6573112 3.3227052
#> 
#> [[3]]
#>  [1] 3.194374 2.140866 2.717488 1.839687 2.601345 3.707104 1.350024 3.608649
#>  [9] 4.441465 3.918482
```

Next, lets pipe the function call into `as_loop()`.

``` r
1:3 %>%
  purrr::map(rnorm, n = 10) %>%
  as_loop()
```

Depending on the automatically detected output settings, the result will
either be

1.  directly inserted in the original R script or the console, given
    that the code is run in RStudio and the {rstudioapi} package is
    installed,
2.  copied to the clipboard, given that the above conditions are not met
    and the {clipr} package is installed,
3.  or if none of the conditions above are met, the result will just be
    printed to the console.

``` r
# --- convert: `map(1:3, rnorm, n = 10)` as loop --- #
.inp1 <- 1:3
out <- vector("list", length = length(.inp1))

for (i in seq_along(.inp1)) {
  out[[i]] <- rnorm(.inp1[[i]], n = 10)
}
# --- end loop --- #
```

To see the result we need to print `out`:

``` r
out
#> [[1]]
#>  [1] -0.7200126  1.7311830  2.2690621 -0.2107278  2.4543609  2.3500439
#>  [7]  2.2721887  1.0647378  0.4952374  1.7298569
#> 
#> [[2]]
#>  [1] 1.3039634 2.7448869 1.2602083 2.9542130 3.2978170 0.7099589 3.2346049
#>  [8] 2.2093101 0.7843484 1.2772277
#> 
#> [[3]]
#>  [1] 1.8397364 3.7574153 0.8742848 2.4375385 4.6123190 1.3133069 1.8128238
#>  [8] 2.7940326 3.5215455 4.5140684
```

## Roadmap and Collaboration

For future versions of `{loopurrr}` the following milestones are
planned:

-   release `{loopurrr}` on CRAN
-   enable support for more iterator functions from `{purrr}`
    (e.g. `cross()` etc.)
-   support base R’s `apply` family in `as_loop()`
-   translate `{purrr}`’s iterators to base R equivalents with
    `as_base()` (yet to be created)

If anyone is interested in collaborating on one or more of those
milestones, any help is appreciated! Feel free to reach out to me, for
example on Twitter
<a href="https://twitter.com/timteafan" target="_blank">@TimTeaFan</a>.

## History

The idea of this package is based on an experience I had at work. After
diving deeper into `{purrr}`’s iterator functions I started refactoring
some old code by replacing loops with `map` functions. To my surprise,
although the code was much cleaner now, not everybody liked it. For some
users it made things harder to understand. Learning once how `map`
functions work, was not enough to solve this, since things got more
complicated when the code was throwing errors. `{loopurrr}` allows us to
write clean code and translate it to regular `for` loops when needed.

## Acknowledgements

Credit goes to the creators and maintainers of the amazing `{purrr}`
package!<br> `{loopurrr}` is just an add-on which would not exist
without it.

Further, credit goes to the
[`{gradethis}`](https://github.com/rstudio/gradethis/%5D) package from
which I adapted [this
code](https://github.com/rstudio/gradethis/blob/main/R/unpipe.R) to make
`as_loop()` work with piped expressions (function calls).
[`{gradethis}`](https://github.com/rstudio/gradethis/blob/main/LICENSE.md)
license and copyrights apply!

I was further inpsired by Miles McBain’s
[`{datapasta}`](https://github.com/MilesMcBain/datapasta)’s different
output options. Looking at the code alone wasn’t enough, I also got help
on
[StackOverflow](https://stackoverflow.com/questions/70572072/how-to-use-indentation-with-rstudioapiinserttext)
from user @Waldi to make the {rstudioapi} package work.

Finally, I adapted [this answer on
StackOverflow](ttps://stackoverflow.com/a/33850689/9349302) to replace
the function arguments of the functions in `map(.f = )` with the actual
objects that are being used.

## Disclaimer

This package does not promote `for` loops over iterator functions.
Rather the opposite is true. I love the `{purrr}` package and would be
happy if people would use it more.

Although this package contains tests with more than 1000 lines of code,
there are definitely a number of edge cases which won’t work correctly.
If you find one, I’d be happy if you file an issue
[here](https://github.com/TimTeaFan/loopurrr/issues).
