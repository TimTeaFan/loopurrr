capture <- function(.f, otherwise = NULL, quiet = TRUE) {
  quietly2(safely2(.f, otherwise = otherwise, quiet = quiet))
}

# adapted from purrr::safely
safely2 <- function(.f, otherwise = NULL, quiet = TRUE) {
  .f <- purrr::as_mapper(.f)
  force(otherwise)
  function(...) attach_error(.f(...), otherwise, quiet)
}

# adapted from purrr:::capture_error
attach_error <- function(code, otherwise = NULL, quiet = TRUE) { #

  tryCatch({
    code
  },
    error = function(e) {
      if (!quiet) {
        message("Error: ", conditionMessage(e))
      }
      i <- capture_env[["i"]]
      capture_env[["out"]][["error_msg"]][[i]] <- conditionMessage(e)
      capture_env[["out"]][["errors"]][[i]]    <- e
      otherwise
      }
  )

}

# adapted from purrr::quietly
quietly2 <- function (.f) {
  .f <- as_mapper(.f)
  function(...) attach_output(.f(...))
}

# adapted from purrr:::capture_output
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
  result <- withCallingHandlers(code,
                                warning = wHandler,
                                message = mHandler)
  output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")

  i <- capture_env$i
  capture_env[["out"]][["output"]][[i]]   <- output
  capture_env[["out"]][["warnings"]][[i]] <- warnings
  capture_env[["out"]][["messages"]][[i]] <- messages

  if (length(capture_env[["out"]][["errors"]]) > i) {
    capture_env[["i"]] <- i + 1L
  }

  result
}

capture_env <- rlang::new_environment(data = list(i = 1L))

prepare_capture_env <- function(i) {
  capture_env[["i"]]                   <- 1L
  capture_env[["out"]]                 <- list()
  capture_env[["out"]][["errors"]]     <- vector(mode = "list", length = i)
  capture_env[["out"]][["error_msg"]]  <- vector(mode = "list", length = i)
  capture_env[["out"]][["output"]]     <- character(i)
  capture_env[["out"]][["warnings"]]   <- vector(mode = "list", length = i)
  capture_env[["out"]][["messages"]]   <- vector(mode = "list", length = i)
}

reset_capture_env <- function() {
  capture_env[["i"]] <- 1L
  rm(out, envir = capture_env)
}

restore <- function(x) {

  out <- tibble::tibble(
    result = x,
    tibble::new_tibble(capture_env[["out"]],
                       nrow = length(capture_env[["out"]][["errors"]]))
    )

  on.exit(reset_capture_env(), add = TRUE)

  return(out)
}
