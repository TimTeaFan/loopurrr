ping <- function(.expr, i = 1L) {

  q <- rlang::enquo(.expr)

  # check and unpipe
  new_expr <- check_and_unpipe(sys.calls(),
                               is_dot = match.call()$`.expr` == ".",
                               calling_fn = "ping")

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

  expr_ls <- as.list(q_ex_std)

  if (!is.null(expr_ls[[".x"]])) {
    expr_ls[[".x"]] <- rlang::call2(`[`, expr_ls[[".x"]], i)
  }

  if (!is.null(expr_ls[[".y"]])) {
    expr_ls[[".y"]] <- rlang::call2(`[`, expr_ls[[".y"]], i)
  }

  if (!is.null(expr_ls[[".l"]])) {

    index_each_el <- rlang::as_function(~ purrr::map(.x, function(x) `[`(x, i)))

    expr_ls[[".l"]] <- rlang::call2(index_each_el, expr_ls[[".l"]])
  }

 eval(as.call(expr_ls))

}

first_and_last <- function(x) {
  ln_x <- length(x)
  if (ln_x > 1) {
    c(1, ln_x)
  } else if (ln_x == 1L) {
    1L
  } else {
    stop("Object must of 1 or more elements.")
  }
}

wrap_.f <- function(.expr, fn) {
  q <- rlang::enquo(.expr)

  # check and unpipe
  new_expr <- check_and_unpipe(sys.calls(),
                               is_dot = match.call()$`.expr` == ".",
                               calling_fn = "wrap_.f")

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

  expr_ls <- as.list(q_ex_std)

  expr_ls[[".f"]] <- rlang::call2(fn, expr_ls[[".f"]])

  eval(as.call(expr_ls))
}
