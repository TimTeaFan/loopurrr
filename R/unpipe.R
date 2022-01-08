# Most parts of this code were copied from:
# https://github.com/rstudio/gradethis/blob/main/R/unpipe.R
# The {gradethis}'s licence and copyrights apply!!
# The code base was slightly adapted to {loopurrr}'s objective



#' Unpipe one layer of piped code.
#'
#' `unpipe()` removes one call to \code{\link[magrittr]{\%>\%}} from an expression,
#' reformatting the result as a nested function call. If the code does not
#' contain a top-level call to \code{\link[magrittr]{\%>\%}}, `unpipe()` returns it as is.
#'
#' @param code a quoted piece of code
#' @noRd
unpipe <- function(code) {

  # Ceci n'est pas une pipe
  if (!is_pipe(code)) return(code)

  # une pipe
  lhs <- code[[2]]
  rhs <- code[[3]]

  if (!is.call(rhs)) {
    # rhs need to be a call
    # mainly because some user do `1 %>% print` instead of `1 %>% print()`
    rhs <-  call(deparse(rhs))
  }


  if (length(rhs) == 1) {
    rhs[[2]] <- lhs
    return(rhs)
  }

  dot <- purrr::map_lgl(as.list(rhs), is_dot)
  if (any(dot)) {
    rhs[[which(dot)]] <- lhs
  } else {
    rhs <- as.call(c(list(rhs[[1]], lhs), as.list(rhs[2:length(rhs)])))
  }
  rhs
}

#' Unpipe nested calls:
unpipe_all <- function(code_expr, .top_level = TRUE) {
  code_expr_len <- length(code_expr)
  if (code_expr_len == 0) return(code_expr)
  if (code_expr_len == 1) return(code_expr)
  if (code_expr_len == 2 && is.null(code_expr[[2]])) return(code_expr)

  re_call <- if (is.pairlist(code_expr)) as.pairlist else as.call
  code_expr <- re_call(purrr::map(as.list(code_expr), unpipe_all, .top_level = FALSE))
  unpipe(code_expr)
}


# Check if call to `as_loop` used pipes, and if, unpipe it.

check_and_unpipe <- function(sc) {

  if (length(sc) > 1) {
    last_cl <- as.list(sc[length(sc) -1L][[1]])
    if (as.character(last_cl[[1]]) == "%>%" && as.character(last_cl[[3]]) == "as_loop") {
      unpipe_all(last_cl[[2]])
    }
  }
}


# Helper functions

is_pipe <- function(x) {
  if (is.call(x) && as.character(x[[1]]) == "%>%") TRUE else FALSE
}

is_dot <- function(name) {
  length(name) == 1 && as.character(name) == "."
}