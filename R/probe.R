# TODO: document and export this function
# TODO: add better error messages
# TODO: add tests

probe <- function(.expr) {

  q <- rlang::enquo(.expr)

  # check and unpipe
  q <- unpipe_expr(q,
                   sc = sys.calls(),
                   is_dot = match.call()$`.expr` == ".",
                   calling_fn = "probe")

  q_expr <- rlang::quo_get_expr(q)
  q_env <- rlang::quo_get_env(q)

  map_fn_chr <- deparse(q_expr[[1]])

  if (grepl("^\\w+::", map_fn_chr, perl = TRUE)) {
    map_fn_chr <- gsub("^\\w+::", "", map_fn_chr)
  }

  map_fn <- get(map_fn_chr, envir = rlang::as_environment("purrr"))
  q_ex_std <- match.call(definition = map_fn, call = q_expr)

  expr_ls <- as.list(q_ex_std)

  obj_x <- eval(expr_ls[[".x"]], envir = q_env)
  has_fn <- !is.null(expr_ls[[".f"]])

  i <- if (has_fn) first_error_imp(q) else NULL

  cl_chr <- call_as_chr(q_expr)

  # doesn't have fn
  if (!has_fn) {
    rlang::inform(c(paste0("Probing call: ", cl_chr),
                    i = "`.f` argument not supplied.",
                    i = "The first element of `.x` is returned as is: `.x[[1]]."))

    return(`[[`(obj_x, 1L))
  }

  # has function and no error
  if (has_fn && is.null(i)) {
    rlang::inform(c(paste0("Probing call: ", cl_chr),
                    i = "No error detected.",
                    i = "Function `.f` applied to the first element in `.x` is returned: `.f(.x[[1]])`."))

    return(ping(q, 1L, simplify = TRUE, `.__impl__.` = TRUE))

  }

  # has_fn and error (!is.null(i)):
  last_call <- check_last_call(sys.calls(), "probe")

  if (last_call) {
    rlang::inform(c(paste0("Probing call: ", cl_chr),
                    i = paste0("The first error is thrown by element no. ", i, "."),
                    i = paste0("This object is returned as is: `.x[[", i, "]]`.")))
  }

  `[[`(obj_x, i)
}
