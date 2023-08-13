capture <- function(.f, otherwise = NULL, quiet = TRUE) {
  quietly2(safely2(.f, otherwise = otherwise, quiet = quiet))
}

safely2 <- function(.f, otherwise = NULL, quiet = TRUE) {
    .f <- purrr::as_mapper(.f)
    force(otherwise)
    function(...) attach_error(.f(...), otherwise, quiet)
}

attach_error <- function(code, otherwise = NULL, quiet = TRUE) {

  tryCatch(
    code,
    error = function(e) {
      if (!quiet) {
        message("Error: ", conditionMessage(e))
      }
      e
    }
  )

}

quietly2 <- function (.f) {
  .f <- as_mapper(.f)
  function(...) attach_output(.f(...))
}

attach_output <- function (code) {
  warnings <- character()
  wHandler <- function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  messages <- character()
  mHandler <- function(m) {
    messages <<- c(messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
  temp <- file()
  sink(temp)
  on.exit({
    sink()
    close(temp)
  })
  result <- withCallingHandlers(code, warning = wHandler, message = mHandler)
  output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")

  if (is.null(result)) {
    out <- list(result)
    attr(out, "capture_unlist") <- TRUE
  } else {
    out <- result
    attr(out, "capture_unlist") <- FALSE
  }
  attr(out, "capture_output")   <- output
  attr(out, "capture_warnings") <- warnings
  attr(out, "capture_messages") <- messages
  out
}

restore_error <- function(x) {
  if (rlang::is_error(x)) x else NULL
}

restore_result <- function(x) {
  if (!rlang::is_error(x)) x else NULL
}

restore <- function(x) {
  map(x, restore_imp)
}

restore_imp <- function(x) {
  output   <- attr(x, "capture_output")
  warnings <- attr(x, "capture_warnings")
  messages <- attr(x, "capture_messages")
  unlist   <- attr(x, "capture_unlist") %||% FALSE

  attributes(x)["capture_output"] <-
    attributes(x)["capture_warnings"] <-
    attributes(x)["capture_messages"] <-
    attributes(x)["capture_unlist"] <- NULL

  result <- if (unlist) {
    unlist(restore_result(x), recursive = FALSE)
    } else {
    restore_result(x)
    }

  error <- if (unlist) {
    unlist(restore_error(x), recursive = FALSE)
  } else {
    restore_error(x)
  }

  list(result   = result,
       error    = error,
       output   = output,
       warnings = warnings,
       messages = messages
  )
}

# list("a", 10, 100) |>
#   map(log)
#
# list("a", 10, 100) |>
#   map(safely(log))
#
# list("a", 10, 100) |>
#   map(safelier(log)) -> x
#
# list(1, 1:3, 1:10) |>
#   accumulate(safelier(sum))
