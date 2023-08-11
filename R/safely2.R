safely2 <- function(.f, otherwise = NULL, quiet = TRUE) {
    .f <- as_mapper(.f)
    force(otherwise)
    purrr:::check_bool(quiet)
    function(...) attach_error(.f(...), otherwise, quiet)
}

attach_error <- function(code, otherwise = NULL, quiet = TRUE) {

  tryCatch(
    code,
    error = function(e) {
      if (!quiet) {
        message("Error: ", conditionMessage(e))
      }

      # out <- e
      # if(!is.null(e)) {
      #    attr(out, "safely2_error") <- e
      # }
      e # out
    }
  )

}

restore_error <- function(x) {
  map(x, restore_error_imp)
}

restore_error_imp <- function(x) {
  if (rlang::is_error(x)) {
    list(result = NULL,
         error = x)
  } else {
    list(result = x,
         error = NULL)
  }
}

restore_all <- function(x) {
  map(x, restore_all_imp)
}

restore_all_imp <- function(x) {
  output   <- attr(x, "quietly2_output")
  warnings <- attr(x, "quietly2_warnings")
  messages <- attr(x, "quietly2_messages")
  unlist   <- attr(x, "quietly2_unlist") %||% FALSE

  attributes(x)["quietly2_output"] <-
    attributes(x)["quietly2_warnings"] <-
    attributes(x)["quietly2_messages"] <-
    attributes(x)["quietly2_unlist"] <- NULL

  result <- if (unlist) {
    unlist(restore_error_imp(x), recursive = FALSE)
    } else {
    restore_error_imp(x)
  }

  list(result   = result,
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
