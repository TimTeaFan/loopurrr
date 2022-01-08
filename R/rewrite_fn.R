rewrite_fn <- function(fn_expr, .inp_objs, .idx, fn_env, cl_chr,
                       .brk = NULL, .dot_args = NULL, is_accu = FALSE, has_init = FALSE,
                       is_back = FALSE, is_redu = FALSE) {

  if (is.null(.brk)) {
    .brk <- list(o = '[[',
                 c = ']]')
  }

  fn <- eval(fn_expr, envir = fn_env)

  # if fn is formula change to normal function
  if (purrr::is_formula(fn)) {
    fn <- purrr::as_mapper(fn)
  }

  is_fun <- rlang::is_function(fn)
  is_lambda <- inherits(fn, "rlang_lambda_function") && !is.name(fn_expr)
  is_anonym <- !is.null(attributes(fn)$srcref) && !is.name(fn_expr)

  # FIXME: lambda vs. anonym
  # if fn is purrr lambda or anonymous function

  if (is_lambda || is_anonym) {

    if (length(.dot_args) != 0) {
      rlang::abort(
        c("Problem with `as_loop()` input `.expr`.",
          i = "`as_loop` does not support argument forwarding to anonymous or purrr-style lambda functions.",
          x = paste0("Additonal arguments have been supplied to the eclipsis `...` in the following call:\n",
                     cl_chr,
                     "\nalthough `.f` is ", if (is_lambda) {"a purrr-style lambda function."} else {"an anonymous function."} ),
          i = "If you want to forward additional arguments in the eclipse `...` of a `map` or similar call, please use a named function, e.g. `mean`, to make it work with `as_loop`.")
      )
    }

    fn_bdy <- trimws(deparse(body(fn)))

    # Still needed?
    if (length(fn_bdy) > 1) {
      fn_bdy <- paste0(if(fn_bdy[1] != "{") "{\n",
                       paste(fn_bdy, collapse = "\n"),
                       if(fn_bdy[1] != "{") "\n}")
    }
  }

  if (is_lambda) {
    idx_suf <- if(is_accu && is_back) 1L else if (!has_init && (is_accu || is_redu)) -1L else 0L

    fn_bdy <- replace_lambda_args(fn_bdy, .inp_objs, .idx, .brk, idx_suf, is_redu, is_back)
    return(fn_bdy)

  } else if (is_anonym) {

    fn_fmls <- rlang::fn_fmls_names(fn)

    if ("..." %in% fn_fmls) {
      rlang::abort(
        c("Problem with `as_loop()` input `.expr`.",
          x = "`as_loop` does not support anonymous functions in `map` or similar calls that use the eclipsis `...` as argument."
        )
      )
    }

    # TODO: check if this stop is still needed:
    # probably the initial check regarding argument forwarding in anonymous functions makes this stop superflous
    stopifnot(length(fn_fmls) == length(.inp_objs))

    for (i in seq_along(.inp_objs)) {

      fn_bdy <- gsub(paste0("(?<!\\w)", fn_fmls[[i]], "(?!\\w)"),
                     paste0(.inp_objs[[i]],
                            if(!(is_redu && !is_back && i == 1L) && !(is_redu && is_back && i == 2L)) {
                              paste0(.brk$o, .idx,
                                     if (is_accu && ((!is_back && i == 1L) || (is_back && i == 2L))) {
                                       "+1"
                                     } else if (i != 2L && (is_accu || is_redu) && !has_init) {
                                       "-1"
                                     },
                                     .brk$c)
                            } # close if is_redu
                     ),
                     fn_bdy,
                     perl = TRUE)
    }
    return(fn_bdy)

    # if fn is just
  } else if (is_fun) {
    if (!length(.dot_args) == 0) {
      dots <- paste0(', ',
                     paste(
                       purrr::imap(.dot_args,
                                   ~ paste0(if (nchar(.y) > 0) paste0(.y, ' = '), .x)),
                       collapse = ", "
                     )
      )
    } else {
      dots <- NULL
    }

    objs_vec <- vector("character", length = length(.inp_objs))

    for (i in seq_along(.inp_objs)) {
      objs_vec[i] <- paste0(.inp_objs[[i]],
                            if(!(is_redu && !is_back && i == 1L) && !(is_redu && is_back && i == 2L)) {
                              paste0(.brk$o, .idx,
                                     if (is_accu && is_back && i == 2L) {
                                       "+1"
                                     } else if (i != 2L && (is_accu || is_redu) && !has_init && !is_back) {
                                       "-1"
                                     },
                                     .brk$c)
                            })
    }
    objs <- paste0(objs_vec, collapse = ", ")
    # objs <- paste0(.inp_objs, .brk$o, .idx, .brk$c, collapse = ", ")
    flag <- !check_syntactical_nm(fn_expr)
    return(paste0(
      if(flag) "`", as.character(fn_expr), if(flag) "`",
      '(', objs, dots,')')
      )
    # all other cases
  } else {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        i = "`as_loop` does not yet support lists, character vectors or numeric vectors supplied as `.f` argument in `map` or similar calls.",
        x = paste0("An object of class <", class(fn), "> was supplied to the `.f` argument in:\n ",cl_chr , "."),
        i = "`as_loop` will work with any function or purrr-style formula in `.f`.")
    )
  }
}



# helper functions -------

check_arg <- function(fn_bdy, arg) {

  grepl(paste0("(?<![^(,\\s])", arg, "(?![^)\\s,])"),
        fn_bdy,
        perl = TRUE)
}

replace_arg <- function(fn_bdy, old_arg, replace_arg, .idx, .brk, idx_suf = 0, is_redu = FALSE, is_back = FALSE, i) {
  gsub(paste0("(?<![^(,\\s])", old_arg, "(?![^)\\s,])"),
       paste0(replace_arg,
              if(!(is_redu && !is_back && i == 1L) && !(is_redu && is_back && i == 2L)) {
                paste0(.brk$o, .idx,
                       if(idx_suf != 0L && ((!is_back && i != 2L) || (is_back && i == 2L))) { # here before i == 1L
                          paste0(if (idx_suf == 1L) "+", idx_suf)
                         },
                       .brk$c)
                }),
       fn_bdy,
       perl = TRUE)
}

loop_replace_args <- function(fn_bdy, old_args, inp, .idx, .brk, idx_suf = 0L, is_redu = FALSE, is_back = FALSE, i) {

  for (j in seq_along(old_args)) {

    if (check_arg(fn_bdy, old_args[j])) {

      return(replace_arg(fn_bdy,
                         old_args[j],
                         inp,
                         .idx,
                         .brk,
                         idx_suf,
                         is_redu,
                         is_back,
                         i = i))
    } # clase if
  } # close loop
}

replace_lambda_args <- function(fn_bdy, inp_ls, .idx, .brk, idx_suf, is_redu, is_back) {

  lamda_fst_arg <- c(".x", "..1", ".")
  lamda_scnd_arg <- c(".y", "..2")

  for (i in seq_along(inp_ls)) {

    if (i == 1L) {
      fn_bdy <- loop_replace_args(fn_bdy,
                                  lamda_fst_arg,
                                  inp_ls[[i]],
                                  .idx,
                                  .brk,
                                  idx_suf = idx_suf,
                                  is_redu = is_redu,
                                  is_back = is_back,
                                  i = i)

    } else if (i == 2L) {

      fn_bdy <- loop_replace_args(fn_bdy,
                                  lamda_scnd_arg,
                                  inp_ls[[i]],
                                  .idx,
                                  .brk,
                                  idx_suf = idx_suf,
                                  is_redu = is_redu,
                                  is_back = is_back,
                                  i = i)

    } else {

      fn_bdy <- replace_arg(fn_bdy,
                            paste0("..", i),
                            inp_ls[[i]],
                            .idx,
                            .brk,
                            idx_suf = idx_suf,
                            i = i)

    } # close if else
  } # close for loop
  fn_bdy
}
