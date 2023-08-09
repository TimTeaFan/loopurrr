# TODO: document and export this function
# TODO: add tests

probe <- function(expr, cond = rlang::is_error) {

  is_error <- identical(rlang::is_error, cond)

  if(!rlang::is_function(
    try(cond <- rlang::as_function(cond),
        silent = TRUE))) {
    rlang::abort(
      c(paste0("Problem with `probe()` input `cond`."),
        i = paste0("`cond` must be either a function name, an anonymous function or a formula that can be coerced to function with `rlang::as_function()`."),
        x = paste0("The input in `cond` doesn't fullful this condition.")
      )
    )
  }

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

  has_fn        <- !is.null(expr_ls[[".f"]])

  is_accu_redu  <- grepl("^(accumulate|reduce)", map_fn_chr, perl = TRUE)
  has_init      <- !is.null(expr_ls[[".init"]])
  is_back       <- !is.null(expr_ls[[".dir"]]) && expr_ls[[".dir"]] == "backward"

  i <- if (has_fn) first_error(!! q, is_back, is_accu_redu, cond) else NULL

  cl_chr <- call_as_chr(q_expr)

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
      "No iteration detected where function in `cond` returned `TRUE`."
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
    paste0("The first iteration where the function in `cond` returned `TRUE` is no. ",
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

first_error <- function(expr, is_back, is_accu_redu, cond) {

  try_fn <- function(.f) {
    function(...) {
      tryCatch({
        .f(...)
      }, error = function(e) {
        e
      })
      }
  }

  q <- rlang::enquo(expr)
  q <- reduce2accumulate(q)

  res <- wrap(!! q,
              .f = try_fn)

  if (is_back) {
    res <- rev(res)
  }

  idx <- tryCatch(vapply(res, cond, FUN.VALUE = logical(1)),
                  error = function(e) e)

  if (rlang::is_error(idx)) {
    msg <- gsub(".*(result is.*)", "\\1", idx$message)
    rlang::abort(
      c(paste0("Problem with `probe()` input `cond`."),
        i = paste0("The function in `cond` must return either `TRUE` or `FALSE`."),
        x = paste0("The ", msg, ".")
      )
    )
  }

  if (any(idx)) {
    which(idx)[1]
  } else (
    NULL
  )

}




