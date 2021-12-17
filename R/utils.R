
# https://stackoverflow.com/a/14295659/9349302
find_calls <- function(x) {
  # Base case
  if (!is.recursive(x)) return()

  recurse <- function(x) {
    sort(unique(as.character(unlist(lapply(x, find_calls)))))
  }

  if (is.call(x)) {
    f_name <- as.character(x[[1]])
    c(f_name, recurse(x[-1]))
  } else {
    recurse(x)
  }
}


check_magrittr_pipe <- function() {
  sc <- sys.calls()
  fn_calls <- find_calls(sc)
  if ("%>%" %in% fn_calls) {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        x = "The magrittr pipe `%>%` is not supported.",
        i = "Please use the base pipe `|>` instead.")
    )
  }
}


names_or_idx <- function(obj, obj_nms) {
  if(is.null(obj_nms)) {
    return(paste0("seq_along(", obj, ")"))
  } else {
    paste0("names(", obj, ")")
  }
}

capture_fn_expr <- function(x) {
  capture.output(print(x))
}

create_inp_objs <- function(obj_ls) {

  comp_obj_ls <- purrr::compact(obj_ls)
  ln <- length(comp_obj_ls)
  res <- vector(mode = "list", length = ln)
  res_nm <- vector(mode = "list", length = ln)
  k <- 1L

  for (i in seq_len(ln)) {
    if (!is.name(obj_ls[[i]])) {
      res_nm[[i]] <- paste0(".inp", k)
      res[[i]] <- deparse_expr(obj_ls[[i]])
      k <- k + 1L
    } else {
      res[[i]] <- res_nm[[i]] <- as.character(obj_ls[[i]])
    }
  }
  purrr::set_names(res, res_nm)
}

create_custom_inpts <- function(obs_ls) {
  if (length(obs_ls) == 0) {
    return(NULL)
  } else {
    out <- purrr::imap(obs_ls, ~ paste0(.y, ' <- ', .x, "\n"))
    paste0(out, collapse = "")
  }
}

get_obj_names <- function(obj, fn_env) {
  obj_vec <- unlist(obj)
  if (names(obj) == obj) {
    return(names(get(obj_vec, envir = fn_env)))
  } else {
    return(names(eval(parse(text = obj_vec), envir = fn_env)))
  }
}

check_arg <- function(fn_bdy, arg) {

  grepl(paste0("(?<![^(,\\s])", arg, "(?![^)\\s,])"),
        fn_bdy,
        perl = TRUE)
}

replace_arg <- function(fn_bdy, old_arg, replace_arg, .idx, .brk) {
  gsub(paste0("(?<![^(,\\s])", old_arg, "(?![^)\\s,])"),
       paste0(replace_arg, .brk$o, .idx, .brk$c),
       fn_bdy,
       perl = TRUE)
}

loop_replace_args <- function(fn_bdy, old_args, inp, .idx, .brk) {

  for (j in seq_along(old_args)) {

    if (check_arg(fn_bdy, old_args[j])) {

      return(replace_arg(fn_bdy,
                         old_args[j],
                         inp,
                         .idx,
                         .brk))
    } # clase if
  } # close loop
}

replace_lambda_args <- function(fn_bdy, inp_ls, .idx, .brk) {

  lamda_fst_arg <- c(".x", "..1", ".")
  lamda_scnd_arg <- c(".y", "..2")

  for (i in seq_along(inp_ls)) {

    if (i == 1L) {
      fn_bdy <- loop_replace_args(fn_bdy,
                                  lamda_fst_arg,
                                  inp_ls[[i]],
                                  .idx,
                                  .brk)

    } else if (i == 2L) {

      fn_bdy <- loop_replace_args(fn_bdy,
                                  lamda_scnd_arg,
                                  inp_ls[[i]],
                                  .idx,
                                  .brk)

    } else {

      fn_bdy <- replace_arg(fn_bdy,
                            paste0("..", i),
                            inp_ls[[i]],
                            .idx,
                            .brk)

    } # close if else
  } # close for loop
  fn_bdy
}

deparse_expr <- function(call) {
  deparse(call,
          width.cutoff = 500L,
          backtick = TRUE,
          nlines = 1L,
          control = NULL)
}

create_out_obj <- function(map_fn, obj, output_nm) {

  map_fn <- gsub("^(imap|pmap|map)2{0,1}_{0,1}", "", map_fn, perl = TRUE)

  mde <- switch(map_fn,
                "chr" = "character",
                "int" = "integer",
                "dbl" = "double",
                "lgl" = "logical",
                "raw" = "raw",
                "list")
  # "dfr" = ,
  # "dfc" = ,
  # "at"  = ,
  # "if"  = "list",
  # "")

  if (!is.null(mde)) {
    vec <- paste0('vector("', mde ,'", length = length(', obj, '))')
    return(paste0(output_nm, ' <- ', vec, '\n\n'))
  }
}


peal_fn <- function(fn) {

  if (as.character(body(fn)[[1]]) == "{") {
    return(body(fn)[[2]])
  } else {
    return(body(fn))
  }
}

call_as_chr <- function(expr) {

  cl <- deparse_expr(expr)
  cl_ticks <- paste0("`", cl, "`")

  if (nchar(cl_ticks) > 50) {
    cl_ticks <- paste0(substr(cl_ticks, 1, 46), '...`')
  }

  cl_ticks
}

bind_rows_cols <- function(map_fn, output_nm, id_arg) {

  do_cl <- switch(map_fn,
                  "map_dfr" = "bind_rows",
                  "map_dfc" = "bind_cols",
                  return(NULL))

  paste0("\n ", output_nm," <- dplyr::", do_cl, "(", output_nm,
         if (do_cl == "bind_rows" && !is.null(id_arg)) {
           paste0(', .id = \"', id_arg, '\"')
         },
         ")\n")

}

finish_lmap <- function(is_lmap, obj, fn_env, output_nm) {

  if(!is_lmap) {
    return(NULL)
  } else {

    flat <- paste0('flatten(', output_nm, ')')

    if (is.data.frame(eval(parse(text = obj), envir = fn_env))) {
      return(paste0('\n', output_nm, ' <- dplyr::as_tibble(', flat, ')\n'))
    } else {
      return(paste0('\n', output_nm, ' <- ', flat, '\n'))
    }
  }
}


call2chr <- function(expr) {
  dep_cl <- deparse(expr, width.cutoff = 70L)
  if (length(dep_cl) > 1 ) {
    del_cl <- paste0(dep_cl, collapse = "\n")
  }
  dep_cl

}


add_selection <- function(map_fn, obj, obj_nms, output_nm, idx, at = NULL, p_fn = NULL, else_fn = NULL) {

  # map_at
  if (grepl("^l{0,1}map_at", map_fn, perl = TRUE) && !is.null(at)) {

    # FIXME: Problem if deparse is used
    is_char <- is.character(eval(at))
    if (!is_char && !is.numeric(eval(at))) {
      stop("unrecognised index type")
    }

    # is char
    if (is_char) {

      # map_at
      if (map_fn == "map_at") {
        return(
          paste0('.at <- ', call2chr(at), '\n',
                 '.sel <- which(names(', obj,') %in% .at)\n')
        )
        # lmap_at
      } else {
        return(
          paste0('.sel <- names(', obj,') %in% ', call2chr(at), '\n')
        )
      }
      # os numeric
    } else {
      # map_at
      if (map_fn == "map_at") {
        return(paste0('.sel <- ', call2chr(at), '\n'))
        # lmap_at
      } else {
        return(paste0('.sel <- seq_along(', obj, ') %in% ', call2chr(at), '\n'))
      }
    }

    # map_if
  } else if (grepl("^l{0,1}map_if", map_fn, perl = TRUE) && !is.null(p_fn)) {
    fn_str <- rewrite_fn(p_fn, obj, idx)
    add_else <- NULL

    if (!is.null(else_fn)) {
      add_else <- rewrite_fn(else_fn, obj, idx)
      else_str <- paste0(output_nm, '[[', idx, ']] <- ', add_else, '\n')
    } else {
      else_str <- paste0('.sel[', idx,'] <- TRUE\n')
    }

    return(paste0('if (!', fn_str, ') {\n',
                  else_str,
                  'next\n', '}\n'))
  }
}


add_at <- function(map_fn, obj, output_nm, idx, at) {

  if (is.null(at)) {
    return(NULL)
  }

  # FIXME: Problem if deparse is used
  is_char <- is.character(eval(at))
  if (!is_char && !is.numeric(eval(at))) {
    stop("unrecognised index type")
  }

  # is char
  if (is_char) {

    # map_at
    if (map_fn == "map_at") {
      return(
        paste0('.at <- ', call2chr(at), '\n',
               '.sel <- which(names(', obj,') %in% .at)\n')
      )
      # lmap_at
    } else {
      return(
        paste0('.sel <- names(', obj,') %in% ', call2chr(at), '\n')
      )
    }
    # os numeric
  } else {
    # map_at
    if (map_fn == "map_at") {
      return(paste0('.sel <- ', call2chr(at), '\n'))
      # lmap_at
    } else {
      return(paste0('.sel <- seq_along(', obj, ') %in% ', call2chr(at), '\n'))
    }
  }
}


add_if <- function(map_fn, obj, output_nm, idx, p_fn, else_fn, brk) {

  if (is.null(p_fn)) {
    return(NULL)
  }

  fn_str <- rewrite_fn(p_fn, obj, idx)
  add_else <- NULL

  if (!is.null(else_fn)) {
    add_else <- rewrite_fn(else_fn, obj, idx, .brk = brk)
    else_str <- paste0(output_nm, '[[', idx, ']] <- ', add_else, '\n')
  } else {
    else_str <- if (map_fn == "lmap_if") {
      paste0(output_nm, '[[', idx, ']] <- ', obj, brk$o, idx, brk$c, '\n')
    } else {
      paste0('.sel[', idx,'] <- TRUE\n')
    }
  }

  return(paste0('if (!', fn_str, ') {\n',
                else_str,
                'next\n', '}\n'))
}



# FIXME: long names in at
# FIXME: integrate lmap in at
# FIXME: use deparse for non-name expressions
add_selection_old <- function(map_fn, obj, obj_nms, output_nm, idx, fn_env, at = NULL, p_fn = NULL, else_fn = NULL) {

  # map_at
  if (grepl("^l{0,1}map_at", map_fn, perl = TRUE) && !is.null(at)) {

    # FIXME: Problem if deparse is used:
    if (is.language(at) && !is.name(at)) {
      at <- eval(at)
    }
    # is name
    if (is.name(at)) {

      at_obj <- get(at, envir = fn_env)

      if (is.character(at_obj)) {

        if (map_fn == "map_at") {
          return(paste0('.sel <- which(names(', obj,') %in% ', at,')\n'))
        } else {
          return(paste0('.sel <- names(', obj,') %in% ', at,')\n'))
        }

      } else {
        if (map_fn == "map_at") {
          return(paste0('.sel <- ', at, '\n'))
        } else {
          return(paste0('.sel <- seq_along(', obj, ') %in% ', at, '\n'))
        }
      }
      # is character
    } else if (is.character(at)) {
      stopifnot(!is.null(obj_nms))

      sel <- obj_nms %in% at

      if (map_fn == "map_at") {
        return(
          paste0('.at <- c(',
                 paste(paste0('"', at, '"'), collapse = ", "),
                 ')\n',
                 '.sel <- which(names(', obj,') %in% .at)\n')
        )} else {
          return(paste0('.sel <- names(', obj,') %in% ',
                        'c(',
                        paste(paste0('"', at, '"'), collapse = ", "),
                        ')\n'))
        }
      # is numeric
    } else {
      if (map_fn == "map_at") {
        return(paste0('.sel <- c(',
                      paste0(at, collapse = ","),
                      ')\n'))
      } else {
        return(paste0('.sel <- seq_along(', obj, ') %in% ',
                      'c(',
                      paste0(at, collapse = ","),
                      ')\n'))
      }
    }
    # map_if
  } else if (grepl("^l{0,1}map_if", map_fn, perl = TRUE) && !is.null(p_fn)) {
    fn_str <- rewrite_fn(p_fn, obj, idx)
    add_else <- NULL

    if(!is.null(else_fn)) {
      add_else <- rewrite_fn(else_fn, obj, idx)
      else_str <- paste0(output_nm, '[[', idx, ']] <- ', add_else, '\n')
    } else {
      else_str <- paste0('.sel[', idx,'] <- TRUE\n')
    }

    return(paste0('if (!', fn_str, ') {\n',
                  else_str,
                  'next\n', '}\n'))
  }
}

rewrite_fn <- function(fn_expr, .inp_objs, .idx, .brk = NULL, .dot_args = NULL) {

  if (is.null(.brk)) {
    .brk <- list(o = '[[',
                 c = ']]')
  }

  fn <- eval(fn_expr)

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
    # fn_bdy <- capture.output(print(peal_fn(fn)))
    fn_bdy <- trimws(deparse(body(fn)))

    # Still needed?
    if (length(fn_bdy) > 1) {
      fn_bdy <- paste0(if(fn_bdy[1] != "{") "{\n",
                       paste(fn_bdy, collapse = "\n"),
                       if(fn_bdy[1] != "{") "\n}")
    }
  }

  if (is_lambda) {

    fn_bdy <- replace_lambda_args(fn_bdy, .inp_objs, .idx, .brk)
    return(fn_bdy)

  } else if (is_anonym) {

    fn_fmls <- rlang::fn_fmls_names(fn)
    stopifnot(!"..." %in% fn_fmls)
    stopifnot(length(fn_fmls) == length(.inp_objs))

    for (i in seq_along(.inp_objs)) {

      fn_bdy <- gsub(paste0("(?<!\\w)", fn_fmls[[i]], "(?!\\w)"),
                     paste0(.inp_objs[[i]], .brk$o, .idx, .brk$c),
                     fn_bdy,
                     perl = TRUE)
    }
    return(fn_bdy)

    # if fn is just
  } else if (is_fun) {
    if (!length(.dot_args) == 0) {
      dots <- paste0(', ',
                     paste(
                       imap(.dot_args, ~ paste0(if (nchar(.y) > 0) paste0(.y, ' = '),
                                                # if (nchar(.y) > 0) ' = ',
                                                .x)),
                       collapse = ", "
                     )
      )
    } else {
      dots <- NULL
    }

    objs <- paste0(.inp_objs, .brk$o, .idx, .brk$c, collapse = ", ")
    return(paste0(as.character(fn_expr),'(', objs, dots,')'))
    # all other cases
  } else {
    stop("Not able to rewrite function.")
  }
}
