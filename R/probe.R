# TODO: document and export this function
# TODO: add better error messages
# TODO: add tests

probe <- function(expr, stop_at = rlang::is_error) {

  # TODO: use rlang::abort instead of `stopifnot`
  stopifnot(rlang::is_function(stop_at))
  is_error <- identical(rlang::is_error, stop_at)

  q <- rlang::enquo(expr)

  # check and unpipe
  q <- unpipe_expr(q,
                   sc = sys.calls(),
                   is_dot = match.call()$expr == ".",
                   calling_fn = "probe")

  q_expr        <- rlang::quo_get_expr(q)
  q_env         <- rlang::quo_get_env(q)

  map_fn_chr    <- deparse_and_rm_nmspace(q_expr[[1]])

  map_fn        <- mabye_check_map_fn(map_fn_chr, "probe", checks = TRUE)

  expr_ls       <- reformat_expr_ls(q_expr = q_expr, fn = map_fn)

  main_obj      <- get_obj(expr_ls, q_env)

  has_fn <- !is.null(expr_ls[[".f"]])

  is_accu_redu  <- grepl("^(accumulate|reduce)", map_fn_chr, perl = TRUE)
  has_init      <- !is.null(expr_ls[[".init"]])
  is_back       <- !is.null(expr_ls[[".dir"]]) && expr_ls[[".dir"]] == "backward"

  i <- if (has_fn) first_error(!! q, is_back, stop_at) else NULL

  cl_chr <- call_as_chr(q_expr)

  expr_ls[[1]] <- if (is_accu_redu) rlang::expr(accumulate2) else rlang::expr(accumulate)

  # doesn't have fn
  if (!has_fn) {
    rlang::inform(c(paste0("Probing call: ", cl_chr),
                    i = "`.f` argument not supplied.",
                    i = "Returning input at first iteration with:",
                    paste0("\033[32m", "\u2714", "\033[39m", " ",
                           deparse(q_expr), " |> peel(1)")
                    )
                  )

    return(peel(!! q, 1L))
  }

  # has function and no error
  if (has_fn && is.null(i)) {

    i_msg <- if (is_error) {
      "No error detected."
    } else {
      "No iteration detected where function in `stop_at` returned `TRUE`."
    }

    rlang::inform(c(paste0("Probing call: ", cl_chr),
                    i = i_msg,
                    i = "Returning output of first iteration with:",
                    paste0("\033[32m", "\u2714", "\033[39m", " ",
                           deparse(q_expr), " |> peek(1)")
                    )
                  )


    return(peek(!! q, 1L))

  }

  adjusted_i <- adjust_i(i, is_accu_redu, has_init) - has_init

  i_msg <- if (is_error) {
    paste0("The first error is thrown at iteration no. ", adjusted_i, ".")
  } else {
    paste0("The first iteration where the function in `stop_at` returned `TRUE` is no. ",
           adjusted_i, ".")
  }

  rlang::inform(c(paste0("Probing call: ", cl_chr),
                  i = i_msg,
                  i = paste0("Returning input at iteration no. ", adjusted_i, " with:"),
                  paste0("\033[32m", "\u2714", "\033[39m", " ",
                         deparse(q_expr), " |> peel(", adjusted_i,")")
                  )
                )

  peel(!! q, adjusted_i)
}



# ------------------------------ #
# probe()'s helper functions ----
# ------------------------------ #

first_error_imp <- function(expr, is_back) {

  try_fn <- function(.f) {
    function(...) try(.f(...), silent = TRUE)
  }

  res <- wrap({{ expr }},
              .f = try_fn)

  if (is_back) {
    res <- rev(res)
  }

  idx <- map_lgl(res, \(x) inherits(x, "try-error"))

  if (any(idx)) {
    which(idx)[1]
  } else (
    NULL
  )

}


first_error <- function(expr, is_back, stop_at) {

  try_fn <- function(.f) {
    function(...) {
      tryCatch({
        .f(...)
      }, error = function(e) {
        e
      })
      }
  }

  res <- wrap({{ expr }},
              .f = try_fn)

  if (is_back) {
    res <- rev(res)
  }

  idx <- map_lgl(res, stop_at)

  if (any(idx)) {
    which(idx)[1]
  } else (
    NULL
  )

}
