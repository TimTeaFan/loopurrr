create_arg_df <- function(.inps_objs, is_lambda, fn_fmls) {

  new_inp_ls <- .inps_objs

  ln <- length(new_inp_ls)
  out <- vector("list", ln)

  if (is_lambda) {
    out <- lapply(seq_len(ln), function(i) {
      if (i == 1L) {
        c(".x", "..1", ".")
      } else if (i == 2L) {
        c(".y", "..2")
      } else {
        paste0("..", i)
      }
    })

    arg_ls <- purrr::imap(out,
                          ~ dplyr::tibble(id = .y,
                                          arg = .x,
                                          inp_nm = .inps_objs[.y]
                          )
    )

    arg_df <- dplyr::bind_rows(arg_ls)
  } else {
    arg_df <- dplyr::tibble(id = seq_along(.inps_objs),
                            arg = fn_fmls,
                            inp_nm = .inps_objs)
  }
  arg_df
}

# slightly adapted from https://stackoverflow.com/a/33850689/9349302
replace_vars <- function(expr, keyvals) {
  if (!length(expr)) return()
  for (i in seq_along(expr)) {
    if (is.call(expr[[i]])) expr[[i]][-1L] <- Recall(expr[[i]][-1L], keyvals)
    if (is.name(expr[[i]]) && deparse(expr[[i]]) %in% names(keyvals)) {
      key_vl <- keyvals[[deparse(expr[[i]])]]
      is_call <- is.call(key_vl)
      expr[[i]] <- if(is_call) {
        key_vl
      } else {
        as.name(key_vl)
      }
    }

  }
  return( expr )
}


replace_all_vars <- function(fn, arg_df, idx, brk_o) {

  fn_bdy <- body(fn)

  for (r in seq_len(nrow(arg_df))) {

    r_sub <- arg_df[r, ]$subset
    rep_var <- as.name(arg_df[r, ]$inp_nm)
    arg_nm <- arg_df[r, ]$arg

    if (!is.na(r_sub)) {

      new_idx <- str2lang(paste0(idx, r_sub))

      rep_var <- call(brk_o, rep_var, new_idx)
    }

    rep_var <- purrr::set_names(list(rep_var), arg_nm)

    fn_bdy <- replace_vars(fn_bdy, rep_var)
  }

  fn_bdy

}

rewrite_fn <- function(fn_expr, .inp_objs, .idx, fn_env, force_eval, cl_chr,
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

  is_fun    <- rlang::is_function(fn)
  is_lambda <- inherits(fn, "rlang_lambda_function") && !is.name(fn_expr)
  is_anonym <- !is.null(attributes(fn)$srcref) && !is.name(fn_expr)

  if (is_lambda || is_anonym) {

    fn_fmls   <- rlang::fn_fmls_names(fn)

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

    if (is_anonym && "..." %in% fn_fmls) {
      rlang::abort(
        c("Problem with `as_loop()` input `.expr`.",
          x = "`as_loop` does not support anonymous functions in `map` or similar calls that use the eclipsis `...` as argument."
        )
      )
    }

    arg_df <- create_arg_df(.inp_objs, is_lambda, fn_fmls)

    # logic that adjusts +/- index for reduce and accumulate
    arg_df <- dplyr::mutate(arg_df, subset = dplyr::case_when(

        (is_redu & !is_back & id == 1L) | (is_redu & is_back & id == 2L) ~ NA_character_,

        is_accu & is_back & id == 2L ~ "+1", # ((!is_back & id == 1L) |

        id != 2L & (is_accu | is_redu) & !has_init  & !is_back ~ "-1",

        TRUE ~ "")
      )

    fn_bdy <- replace_all_vars(fn = fn, arg_df = arg_df, idx = .idx, brk_o = .brk$o)

    # TODO: add replacement of i with `.(i)` here:
    if (force_eval) {
      new_idx <- str2lang(paste0(".(", .idx, ")"))
      rep_var <- purrr::set_names(list(new_idx), .idx)
      fn_bdy <- replace_vars(fn_bdy, rep_var)
    }

    return(deparse(fn_bdy))

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

    flag <- !check_syntactical_nm(fn_expr)
    return(paste0(
      if(flag) "`", as.character(fn_expr), if(flag) "`",
      '(', objs, dots,')')
    )
    # extractor function with numeric or character vectors:
  } else if (is.numeric(fn) || is.character(fn)) {
    stopifnot(length(.inp_objs) == 2)

    extr_str <- paste0('tryCatch({',
                       .inp_objs[[1]], .brk$o, .idx, .brk$c, '[[', .inp_objs[[2]], ']]\n',
                       '}, error = function(e) {})')

    return(extr_str)

    # all other cases
  } else {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        i = "`as_loop` does not yet support lists when supplied as `.f` argument in `map` or similar calls.",
        x = paste0("An object of class <", class(fn), "> was supplied to the `.f` argument in:\n ",cl_chr , "."),
        i = "`as_loop` will work with any function, purrr-style formula, numeric or character vectors in `.f`.")
    )
  }
}
