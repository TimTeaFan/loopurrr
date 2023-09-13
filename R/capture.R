capture <- function(.f, otherwise = NULL, quiet = TRUE, env = FALSE) {

  if(env) {
    return(quietly2(safely2(.f, otherwise = otherwise, quiet = quiet)))
  } else {
    return(quietly(safely(.f, otherwise = otherwise, quiet = quiet)))
  }

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
      capture_env[["out"]][["error"]][[i]]     <- e
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

  if (length(capture_env[["out"]][["error"]]) > i) {
    capture_env[["i"]] <- i + 1L
  }

  result
}

capture_env <- rlang::new_environment(data = list(i = 1L))

prepare_capture_env <- function(i) {
  capture_env[["i"]]                   <- 1L
  capture_env[["out"]]                 <- list()
  capture_env[["out"]][["error"]]      <- vector(mode = "list", length = i)
  capture_env[["out"]][["error_msg"]]  <- vector(mode = "list", length = i)
  capture_env[["out"]][["output"]]     <- character(i)
  capture_env[["out"]][["warnings"]]   <- vector(mode = "list", length = i)
  capture_env[["out"]][["messages"]]   <- vector(mode = "list", length = i)
}

reset_capture_env <- function() {
  capture_env[["i"]] <- 1L
  rm(out, envir = capture_env)
}

restore <- function(x, has_init, is_back, env = FALSE) {

  if (env) {
    on.exit(reset_capture_env(), add = TRUE)

    cap_res <- tibble::new_tibble(capture_env[["out"]],
                                  nrow = length(capture_env[["out"]][["error"]]))

    if(!has_init) {
      if (is_back) {
        cap_res <- dplyr::bind_rows(cap_res, empty_row)
      } else {
        cap_res <- dplyr::bind_rows(empty_row, cap_res)
      }
    }

    out <- tibble::tibble(
      result = x,
      cap_res
      )

    return(out)
  }

  new_x <- transpose(x)

  safely_res <- transpose(transpose(x)[["result"]])

  new_x[["result"]]     <- safely_res[["result"]]
  new_x[["error"]]      <- safely_res[["error"]]
  new_x[["error_msg"]]  <- map(safely_res[["error"]], "message")

  out <- tibble::new_tibble(new_x,
                            nrow = length(new_x[["result"]])
                            )

  return(out)
}

empty_row <- tibble::tibble(
  error     = list(NULL),
  error_msg = list(NULL),
  output    = "",
  warnings  = list(character(0)),
  messages  = list(character(0))
  )
