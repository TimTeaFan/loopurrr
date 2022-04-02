#' Get and set output loopurrr's options
#'
#' @description
#' `get_output_opt()` and `set_output_opt()` get and set loopurrr's output options. They are light
#' wrappers around `getOption("loopurrr.output")` and `options("loopurrr.output")`.
#'
#' `default_context()` inspects if the `"loopurrr.output"` option is set. If the option is not
#' specified, it will default to `c("rstudio", "clipboard", "console")`. If `"console"` is not
#' among the output options it will be automatically included as last option.
#'
#' @param x Either `NULL` or one or several of `"rstudio"`, `"clipboard"`, `"console"`. If set to
#' more than one option, the output options will be run in order from left to right until
#' successful.
#'
#' @param default If the specified option is not set in the options list, this value is returned.
#' This argument is for internal use only.
#'
#' @returns
#' For `get_output_opt()`, the current value set for option `"loopurrr.output"`, or default
#' (which defaults to NULL) if the option is unset.
#'
#' For `default_context()`, either the current value set for option `"loopurrr.output"`.
#' In this case, if `"console"` is not among the options, it will be automatically included as last
#' option. Or, if option `"loopurrr.output"` is not specified `c("rstudio", "clipboard", "console")`.
#'
#'
#' @section Examples:
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' set_output_opt(c("clipboard", "rstudio"))
#'
#' get_ouptut_opt()
#' #> [1] "clipboard" "rstudio"
#'
#' default_context()
#' #> [1] "clipboard" "rstudio" "console"
#' ```
#'
#' @rdname output_options
#' @export
get_output_opt <- function(default = NULL) {
  getOption("loopurrr.output", default = default)
}

#' @rdname output_options
#' @export
set_output_opt <- function(x = list("rstudio", "clipboard", "console", NULL)) {
  match.arg(x, several.ok = TRUE)
  quoted_option <- bquote(options("loopurrr.output" = .(x)))
  option_chr <- gsub('"', "'", deparse(quoted_option))
  x_chr <- gsub('"', "'", deparse(x))
  eval(quoted_option)
  rlang::inform(paste0('{loopurrr}s output option has been temporarily set to: ', x_chr, '.',
                       if(all(!is.null(x))) {
                         paste0('\nTo set this option permanently add `', option_chr, '` to your .Rprofile.')
                       })
  )
}

#' @rdname output_options
#' @export
default_context <- function() {
  out_opt <- get_output_opt(default = c("rstudio", "clipboard"))
  if(!"console" %in% out_opt) {
    out_opt <- c(out_opt, "console")
  }
  out_opt
}


#' depending on output_context check and create final output function
create_output_fn <- function(output) {

  has_rstudioapi <- requireNamespace("rstudioapi", quietly = TRUE)
  has_clipr <- requireNamespace("clipr", quietly = TRUE)

  is_rstudio <- is_clipr <- NULL

  out_fn <- NULL

  for (i in seq_along(output)) {

    if (output[i] == "rstudio" && has_rstudioapi) {

      is_rstudio <- rstudioapi::isAvailable()
      if (is_rstudio) {
        out_fn <- function(x) {
          insert_and_reformat_text(x)
        }
      }
      break

    } else if (output[i] == "clipboard" && has_clipr) {

      is_clipr <- clipr::clipr_available()
      if (is_clipr) {
        out_fn <- function(x) {
          clipr::write_clip(content = x, object_type = "character")
          rlang::inform("The translated function was copied to the clipboard.")
        }
      }
      break

    } else {
      out_fn <- base::cat
      break
    }
  }

  # create a meaningful error message:
  if (is.null(out_fn)) {

    rstudio_msg <- if (!has_rstudioapi) {
      "the {rstudioapi} package is not installed."
    } else if (!is_rstudio) {
      "your are not in RStudio."
    }

    clipr_msg <- if (!has_clipr) {
      "the {clipr} package is not installed."
    } else if (!is_clipr) {
      "the system clipboard is not accessible."
    }

    err_msg <- if (length(output) == 2L) {
      paste0("'rstudio' and 'clipboard' were specified as output argument. Unfortunately, ", rstudio_msg, "and ", clipr_msg)
    } else {
      if (output == "rstudio") {
        paste0("'rstudio' was specified as `output` argument, but ", rstudio_msg)
      } else {
        paste0("'clipboard' was specified as output argument, but ", clipr_msg)
      }
    }

    rlang::abort(
      c("Problem with `as_loop()` input `output`.",
        i = paste0("The specified output ", if(length(output) == 1L) "option is" else "options are", " not supported."),
        x = err_msg,
        i = "Please refrain from specifying the output argument or set it to 'console' to make `as_loop` work.")
    )
  }

  out_fn
}


