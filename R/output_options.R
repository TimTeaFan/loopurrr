
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
          # if (requireNamespace("styler", quietly = TRUE)) {
          #   x <- styler::style_text(x, indent_by = 2)
          # }
          # rstudioapi::insertText(text = append(x, "\n"))
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

get_output_opt <- function(default = NULL) {
  getOption("loopurrr.output", default = default)
}

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

default_output <- function() {
  out_opt <- get_output_opt(default = c("rstudio", "clipboard"))
  if(!"console" %in% out_opt) {
    out_opt <- c(out_opt, "console")
  }
  out_opt
}
