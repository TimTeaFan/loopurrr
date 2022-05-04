# we need ping to rewrite check_lazy
ping <- function(.expr, i) {

  q <- rlang::enquo(.expr)

  new_expr <- check_and_unpipe(sys.calls(), is_dot = match.call()$`.expr` == ".")
  if (!is.null(new_expr)) {
    q <- rlang::quo_set_expr(q, new_expr)
  }

  q_expr <- rlang::quo_get_expr(q)
  q_env <- rlang::quo_get_env(q)

  map_fn_chr <- deparse(q_expr[[1]])

  if (grepl("^\\w+::", map_fn_chr, perl = TRUE)) {
    map_fn_chr <- gsub("^\\w+::", "", map_fn_chr)
  }

  map_fn <- get(map_fn_chr, envir = rlang::as_environment("purrr"))
  q_ex_std <- match.call(definition = map_fn, call = q_expr)

  fn_expr <- expr_ls[[".f"]]
  fn <- eval(fn_expr, envir = fn_env)

  x_arg   <- eval(expr_ls[[".x"]], envir = q_env)
  x_arg

}

library(purrr)
x <- list(2:3, 5:6, 7:8)
map(x, sum)

map(x, sum) %>%
  ping(1)
