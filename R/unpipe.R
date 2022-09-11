# Most parts of this code were copied from:
# https://github.com/rstudio/gradethis/blob/main/R/unpipe.R
# The {gradethis}'s licence and copyrights apply!!
# The code base was slightly adapted to {loopurrr}'s objective

# Unpipe one layer of piped code.
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

# Unpipe nested calls:
# TODO: add stop that stops preventing `.f` from getting unpiped
unpipe_all <- function(code_expr, .top_level = TRUE) {
  code_expr_len <- length(code_expr)
  if (code_expr_len == 0) return(code_expr)
  if (code_expr_len == 1) return(code_expr)
  if (code_expr_len == 2 && is.null(code_expr[[2]])) return(code_expr)

  re_call <- if (is.pairlist(code_expr)) as.pairlist else as.call
  code_expr <- re_call(purrr::map(as.list(code_expr), unpipe_all, .top_level = FALSE))
  unpipe(code_expr)
}

# Helper functions
is_pipe <- function(x) {
  if (is.call(x) && as.character(x[[1]])[[1]] == "%>%") TRUE else FALSE
}

is_dot <- function(name) {
  length(name) == 1 && as.character(name) == "."
}


# replace piped expression

unpipe_expr <- function(quo_expr, sc, is_dot, calling_fn) {
  if (is_dot) {
    piped_call <- find_piped_call(sc, calling_fn)
    new_expr <- unpipe_all(piped_call)
    if (!is.null(new_expr)) {
      quo_expr <- rlang::quo_set_expr(quo_expr, new_expr)
    }
  }
  quo_expr
}


find_piped_call <- function(call, calling_fn, target = NULL) {
  if (is.null(target)) {
    target <- environment()
  }
  out <- NULL
  for (cl in call) {
    cl_ls <- as.list(cl)
    cl_ln <- length(cl_ls)
    if (is.symbol(cl_ls)) next
    if (cl_ln == 3) {
      if (as.character(cl_ls[[1]])[[1]] == "%>%" && as.character(cl_ls[[3]])[[1]] == calling_fn) {
        res <- cl_ls[[2]]
        assign("out", res, envir = target)
      }
      Recall(cl_ls, calling_fn, target = target)
    }
  }
  out
}
