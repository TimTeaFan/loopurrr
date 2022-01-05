
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

create_output_fn <- function(output) {

  has_rstudioapi <- requireNamespace("rstudioapi", quietly = TRUE)
  has_clipr <- requireNamespace("clipr", quietly = TRUE)

  is_rstudio <- is_clipr <- NULL

  out_fn <- NULL

  for (i in seq_along(output)) {

    if (output[i] == "rstudio" && has_rstudioapi) {

      is_rstudio <- rstudioapi::isAvailable()
      if (is_rstudio) {
        out_fn <- function(x) {
          if (requireNamespace("styler", quietly = TRUE)) {
            x <- styler::style_text(x, indent_by = 2)
          }
          rstudioapi::insertText(text = append(x, "\n"))
          }
        }
      break

    } else if (output[i] == "clipboard" && has_clipr) {

      is_clipr <- clipr::clipr_available()
      if (is_clipr) {
        out_fn <- function(x) {
          clipr::write_clip(content = x, object_type = "character")
          rlang::inform("The translated function was copied to the clipboard.")
        }
      }
      break

    } else {
      out_fn <- base::cat
      break
    }
  }

  # create a meaningful error message:
  if (is.null(out_fn)) {

    rstudio_msg <- if (!has_rstudioapi) {
      "the {rstudioapi} package is not installed."
    } else if (!is_rstudio) {
      "your are not in RStudio."
    }

    clipr_msg <- if (!has_clipr) {
      "the {clipr} package is not installed."
    } else if (!is_clipr) {
        "the system clipboard is not accessible."
      }

    err_msg <- if (length(output) == 2L) {
      paste0("'rstudio' and 'clipboard' were specified as output argument. Unfortunately, ", rstudio_msg, "and ", clipr_msg)
    } else {
      if (output == "rstudio") {
        paste0("'rstudio' was specified as `output` argument, but ", rstudio_msg)
      } else {
        paste0("'clipboard' was specified as output argument, but ", clipr_msg)
      }
    }

    rlang::abort(
      c("Problem with `as_loop()` input `output`.",
        i = paste0("The specified output ", if(length(output) == 1L) "option is" else "options are", " not supported."),
        x = err_msg,
        i = "Please refrain from specifying the output argument or set it to 'console' to make `as_loop` work.")
    )
  }

  out_fn
}

# get_supported_fns <- function() {
#   require(purrr)
#   all_purrr_fns <- purrr::map_chr(lsf.str("package:purrr"), `[`)
#   purrr_map_fns <- grep("(map2)|(modify)|(walk)|(map_)|(map)$", all_purrr_fns, value = TRUE)
#   invoke_fns <- grepl("^invoke_", purrr_map_fns)
#   call_or_depth_fns <- grepl("(_call)|(_depth)$", purrr_map_fns)
#   sel_map_fns <- purrr::set_names(purrr_map_fns[!(invoke_fns | call_or_depth_fns)])
#
#   not_include <- c("modify_in", "list_modify", "lmap_if")
#
#   sel_map_fns[! sel_map_fns %in% not_include]
# }
#
# datapasta::vector_paste(get_supported_fns())

is_supported <- function(map_fn) {

  supported_fns <- c("imap", "imap_chr", "imap_dbl", "imap_dfc", "imap_dfr", "imap_int", "imap_lgl",
                     "imap_raw", "imodify", "iwalk", "lmap", "lmap_at", "map", "map_at", "map_chr",
                     "map_dbl", "map_df", "map_dfc", "map_dfr", "map_if", "map_int", "map_lgl",
                     "map_raw", "map2", "map2_chr", "map2_dbl", "map2_df", "map2_dfc", "map2_dfr",
                     "map2_int", "map2_lgl", "map2_raw", "modify", "modify_at", "modify_if",
                     "modify2", "pmap", "pmap_chr", "pmap_dbl", "pmap_df", "pmap_dfc", "pmap_dfr",
                     "pmap_int", "pmap_lgl", "pmap_raw", "pwalk", "walk", "walk2", "accumulate")

  if (!any(purrr::map_lgl(findFunction(map_fn), ~rlang::env_name(.x) == "package:purrr"))) {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        i = "`as_loop` only works with `map` and similar functions from the purrr package.",
        x = paste0("`", map_fn, "` is not located in the namespace of `package:purrr`."),
        i = "For an overview of all currently supported purrr functions see the documentation `?as_loop`.")
    )
  }

  if (!map_fn %in% supported_fns) {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        i = "Currently `as_loop` does only support certain {purrr} functions.",
        x = paste0("`", map_fn, "` is not supported yet."),
        i = "For an overview of all currently supported purrr functions see the documentation `?as_loop`.")
    )
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

get_output_opt <- function(default = NULL) {
  getOption("loopurrr.output", default = default)
}

set_output_opt <- function(x = list("rstudio", "clipboard", "console", NULL)) {
  match.arg(x, several.ok = TRUE)
  quoted_option <- bquote(options("loopurrr.output" = .(x)))
  option_chr <- gsub('"', "'", deparse(quoted_option))
  x_chr <- gsub('"', "'", deparse(x))
  eval(quoted_option)
  rlang::inform(paste0('{loopurrr}s output option has been temporarily set to: ', x_chr, '.',
                       if(all(!is.null(x))) {
                         paste0('\nTo set this option permanently add `', option_chr, '` to your .Rprofile.')
                         })
                )
}

default_output <- function() {
  out_opt <- get_output_opt(default = c("rstudio", "clipboard"))
  if(!"console" %in% out_opt) {
    out_opt <- c(out_opt, "console")
  }
  out_opt
}

names_or_idx <- function(obj, obj_nms) {
  if(is.null(obj_nms)) {
    return(paste0("seq_along(", obj, ")"))
  } else {
    paste0("names(", obj, ")")
  }
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

replace_arg <- function(fn_bdy, old_arg, replace_arg, .idx, .brk, minus_one = FALSE) {
  gsub(paste0("(?<![^(,\\s])", old_arg, "(?![^)\\s,])"),
       paste0(replace_arg, .brk$o, .idx,
              if(minus_one) "-1L", .brk$c),
       fn_bdy,
       perl = TRUE)
}

loop_replace_args <- function(fn_bdy, old_args, inp, .idx, .brk, minus_one = FALSE) {

  for (j in seq_along(old_args)) {

    if (check_arg(fn_bdy, old_args[j])) {

      return(replace_arg(fn_bdy,
                         old_args[j],
                         inp,
                         .idx,
                         .brk,
                         minus_one))
    } # clase if
  } # close loop
}

replace_lambda_args <- function(fn_bdy, inp_ls, .idx, .brk, minus_one) {

  lamda_fst_arg <- c(".x", "..1", ".")
  lamda_scnd_arg <- c(".y", "..2")

  for (i in seq_along(inp_ls)) {

    if (i == 1L) {
      fn_bdy <- loop_replace_args(fn_bdy,
                                  lamda_fst_arg,
                                  inp_ls[[i]],
                                  .idx,
                                  .brk,
                                  minus_one = minus_one)

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

create_out_obj <- function(map_fn, obj, output_nm, has_init) {

  if (grepl("walk", map_fn)) {
    return(NULL)
  }

  if (grepl("modify", map_fn)) {
    return(NULL) # return(paste0(output_nm, ' <- ', obj, '\n\n'))
  }

  map_fn <- gsub("^p{0,1}map2{0,1}_{0,1}", "", map_fn, perl = TRUE)

  mde <- switch(map_fn,
                "chr" = "character",
                "int" = "integer",
                "dbl" = "double",
                "lgl" = "logical",
                "raw" = "raw",
                "list")

  if (grepl("(reduce|accumulate)", map_fn)) {
    mde <- paste0("mode(", obj, ")")
    lng <- paste0("length(", obj, if (has_init) "+1L", ")")
  } else {
    mde <- paste0('"', mde ,'"')
    lng <- paste0("length(", obj, ")")
  }

  if (!is.null(mde)) {
    vec <- paste0('vector(', mde,', length = ', lng, ')')
    return(paste0(output_nm, ' <- ', vec, '\n'))
  }
}

create_init <- function(init, has_init) {
  if (!has_init) {
    return(NULL)
  } else {
    # FIXME: if call then deparse!
    paste0(".init <- ", init, "\n")
  }
}

prep_accu_out <- function(map_fn_chr, obj, output_nm, init, has_init) {

  if (!grepl("reduce|accumulate", map_fn_chr)) {
    return(NULL)
  }
  if (has_init) {
    .init <- if (is.name(init)) {
      as.character(init)
    } else {
      deparse_expr(init)
    }
    return(paste0(output_nm, '[[1]]', ' <- ', .init, '\n'))
  } else {
  paste0(output_nm, '[[1]]', ' <- ', obj, '[[1]]', '\n')
  }
}

create_assign <- function(map_fn, output_nm, obj, idx, is_accu, has_init) {

  if(grepl("walk", map_fn)) {
    return(NULL)
  }
  # if(grepl("modify", map_fn)) {
  #   output_nm <- obj
  # }
  paste0(output_nm, '[[', idx,
         if (is_accu && has_init) '+1L', ']] <- ')

}

create_obj_names <- function(obj, output_nm, obj_nms, is_lmap, is_modify, is_walk, is_accu, has_init) {
  if (is.null(obj_nms) || is_lmap || is_modify || is_walk) {
    return(NULL)
  }
  nms <- paste0('names(', obj, ')')
  if (is_accu && has_init) {
    nms <- paste0('c(".init",', nms, ')')
  }
  paste0('\nnames(', output_nm, ') <- ', nms, '\n')
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
                  "map2_dfr" = ,
                  "pmap_dfr" = ,
                  "map_dfr"  = "bind_rows",
                  "map2_dfc" = ,
                  "pamp_dfc" = ,
                  "map_dfc"  = "bind_cols",
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
    if (map_fn %in% c("map_at", "modify_at")) {
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
    if (map_fn %in% c("map_at", "modify_at")) {
      return(paste0('.sel <- ', call2chr(at), '\n'))
      # lmap_at
    } else {
      return(paste0('.sel <- seq_along(', obj, ') %in% ', call2chr(at), '\n'))
    }
  }
}


add_if <- function(map_fn, obj, output_nm, idx, p_fn, else_fn, brk, fn_env) {

  if (is.null(p_fn)) {
    return(NULL)
  }

  fn_str <- rewrite_fn(p_fn, obj, idx, fn_env)
  add_else <- NULL

  if (!is.null(else_fn)) {
    add_else <- rewrite_fn(else_fn, obj, idx, fn_env, .brk = brk)
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

rewrite_fn <- function(fn_expr, .inp_objs, .idx, fn_env, cl_chr,
                       .brk = NULL, .dot_args = NULL, is_accu = FALSE, has_init = FALSE) {

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

    fn_bdy <- replace_lambda_args(fn_bdy, .inp_objs, .idx, .brk, minus_one = is_accu && !has_init)
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
                     paste0(.inp_objs[[i]], .brk$o, .idx,
                            if(is_accu && !has_init && i == 1L) "-1L",#
                            .brk$c),
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
                                                .x)),
                       collapse = ", "
                     )
      )
    } else {
      dots <- NULL
    }

    objs_vec <- vector("character", length = length(.inp_objs))
    for (i in seq_along(.inp_objs)) {
      objs_vec[i] <- paste0(.inp_objs[[i]], .brk$o, .idx,
                            if(is_accu && !has_init && i == 1L) "-1L",
                            .brk$c)
    }
    objs <- paste0(objs_vec, collapse = ", ")
    # objs <- paste0(.inp_objs, .brk$o, .idx, .brk$c, collapse = ", ")
    return(paste0(as.character(fn_expr),'(', objs, dots,')'))
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
