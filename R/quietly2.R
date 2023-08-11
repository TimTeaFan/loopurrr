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
    attr(out, "quietly2_unlist") <- TRUE
  } else {
    out <- result
    attr(out, "quietly2_unlist") <- FALSE
  }
  attr(out, "quietly2_output")   <- output
  attr(out, "quietly2_warnings") <- warnings
  attr(out, "quietly2_messages") <- messages
  out
}

restore_output <- function(x) {
  map(x, restore_output_imp)
}

restore_output_imp <- function(x) {
  output   <- attr(x, "quietly2_output")
  warnings <- attr(x, "quietly2_warnings")
  messages <- attr(x, "quietly2_messages")
  unlist   <- attr(x, "quietly2_unlist") %||% FALSE

  attributes(x)["quietly2_output"] <-
    attributes(x)["quietly2_warnings"] <-
    attributes(x)["quietly2_messages"] <-
    attributes(x)["quietly2_unlist"] <- NULL

  list(result   = if (unlist) unlist(x, recursive = FALSE) else x,
       output   = output,
       warnings = warnings,
       messages = messages
  )
}
