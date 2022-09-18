# TODO: document and export this function
# TODO: add better error messages
# TODO: add tests

# Tests:
# a <- list(1, 1:2, 1:3)
# map(a, sum) %>% ping(1)
# map(a, sum) %>% ping(c(1,3))
# map(a, sum) %>% ping(simplify = FALSE)
# ping rewritten based on wrap
ping <- function(.expr, i = 1L, simplify = TRUE, ...) {

  dots <- rlang::list2(...)

  # checks if ping is used to implement another function
  implement <- !is.null(dots[[".__impl__."]])

  if (implement) {
    q <- .expr
  } else {
    q <- rlang::enquo(.expr)

    # check and unpipe
    q <- unpipe_expr(q,
                     sc = sys.calls(),
                     is_dot = match.call()$`.expr` == ".",
                     calling_fn = "ping")
  }

  check_names <- check_i(i, "ping")

  out <- wrap(.expr = q,
              .x = index_fn(i),
              .y = index_fn(i),
              .l = index_each_fn(i),
              silent = TRUE,
              `.__impl__.` = TRUE,
              `.__check__.`= check_names
  )

  if (simplify && is.list(out) && length(out) == 1L) {
    out <- unlist(out, recursive = FALSE)
  }

  out

}
