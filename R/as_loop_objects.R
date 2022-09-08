names_or_idx <- function(obj, obj_nms) {
  if(is.null(obj_nms)) {
    return(paste0("seq_along(", obj, ")"))
  } else {
    paste0("names(", obj, ")")
  }
}

create_inp_ls <- function(fn_expr, l_arg, x_arg, y_arg, is_extr_fn) {

  # if pmap
  if (!is.null(l_arg)) {
    inp_ls <- as.list(l_arg[-1])
  # if extractor function
  } else if (is_extr_fn) {
    inp_ls <- list(.x = x_arg,
                   .y = fn_expr)
  # all other cases
  } else {
    inp_ls <- list(.x = x_arg,
                   .y = y_arg)
  }
  inp_ls
}

create_inp_objs <- function(obj_ls, output_nm, idx, is_modify, is_i, is_accu, is_redu, is_back, q_env) {

  par_frame <- parent.frame()
  comp_obj_ls <- purrr::compact(obj_ls)
  symb_chr_ls <- as.character(purrr::keep(comp_obj_ls, is.symbol))

  ln <- length(comp_obj_ls)
  res <- vector(mode = "list", length = ln)
  res_nm <- vector(mode = "list", length = ln)
  k <- 1L

  for (i in seq_len(ln)) {
    if (!is.name(obj_ls[[i]])) {
      while (paste0(".inp", k) %in% symb_chr_ls) {
        k <- k + 1
      }
      res_nm[[i]] <- paste0(".inp", k)
      res[[i]] <- deparse_expr(obj_ls[[i]])
      k <- k + 1L
    } else {
      res[[i]] <- res_nm[[i]] <- as.character(obj_ls[[i]])
    }
  }

  inp_ls <- purrr::set_names(res, res_nm)

  # check if output name in input list
  if (output_nm %in% names(inp_ls)) {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        x = paste0('Input object must not have the same variable name as output: "', output_nm, '".'),
        i = "Please rename `as_loop()`'s `output_nm` argument to an output name which is not used as input.")
    )
  }

  # check if index name in input list
  if (idx %in% names(inp_ls)) {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        x = paste0('Input object must not have the same variable name as index: "', idx, '".'),
        i = "Please rename `as_loop()`'s `idx` argument to a name which is not used as input.")
    )
  }

  # assign bare input names to parent enviroment before changing them
  assign("bare_inp_nms",
         names(inp_ls),
         par_frame)

  if (!is.null(inp_ls) && is_modify) {
    names(inp_ls)[1] <- output_nm
  }

  # create object names and assign to parent environment
  obj_nms <- get_obj_names(inp_ls[1], q_env)
  assign("obj_nms",
         obj_nms,
         par_frame)

  # create first obj name and assign to parent environment
  obj <- names(inp_ls)[1]
  assign("obj",
         obj,
         par_frame)

  # if imap
  if (is_i) {
    inp_ls <- append(inp_ls,
                     list(.idx = names_or_idx(obj, obj_nms)))
  }
  # if accumlate or reduce
  if (is_accu || is_redu) {
    inp_ls <- if (!is_back) {
      purrr::prepend(inp_ls,
                     rlang::list2("{output_nm}" := output_nm))
    } else {
      append(inp_ls,
             rlang::list2("{output_nm}" := output_nm),
             after = 1)
    }
  }
  inp_ls
}

create_custom_inpts <- function(obs_ls) {
  obs_ls <- obs_ls[names(obs_ls) != obs_ls]

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


create_out_obj <- function(map_fn, obj, output_nm, has_init, init, is_back) {

  if (grepl("walk", map_fn)) {
    return(NULL)
  }

  if (grepl("modify", map_fn)) {
    return(NULL)
  }

  # when reduce
  if (grepl("reduce", map_fn)) {
    if (has_init) {
      idx <- NULL
      vec <- if (is.name(init)) {
        as.character(init)
      } else {
        deparse_expr(init)
      }
    } else {
      idx <- if(!is_back) "[[1]]" else paste0("[[length(", obj, ")]]")
      vec <- obj
    }
    return(paste0(output_nm, ' <- ', vec, idx, '\n'))
  }

  map_fn <- gsub("^p{0,1}map2{0,1}_{0,1}", "", map_fn, perl = TRUE)

  mde <- switch(map_fn,
                "chr" = "character",
                "int" = "integer",
                "dbl" = "double",
                "lgl" = "logical",
                "raw" = "raw",
                "list")

  # when accumulate
  if (grepl("^accumulate$", map_fn)) {
    # mde <- paste0("mode(", obj, ")")
    lng <- paste0("length(", obj, ")", if (has_init) "+1L")
  } else {
    # mde <- paste0('"', mde ,'"')
    lng <- paste0("length(", obj, ")")
  }

  if (!is.null(mde)) {
    vec <- paste0('vector(', '"', mde, '"',', length = ', lng, ')')
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


prep_accu_out <- function(map_fn_chr, obj, output_nm, init, has_init, is_back) {

  if (!grepl("accumulate", map_fn_chr)) {
    return(NULL)
  }

  idx <- if (is_back) paste0('length(', obj, ')') else '1'
  idx_suf <- if (has_init && is_back) "+1" else NULL

  first_or_last <- paste0('[[', idx, idx_suf,']]')

  if (has_init) {
    .init <- if (is.name(init)) {
      as.character(init)
    } else {
      deparse_expr(init)
    }
    return(paste0(output_nm, first_or_last, ' <- ', .init, '\n'))
  } else {
    paste0(output_nm, first_or_last, ' <- ', obj, '[[', idx, ']]', '\n')
  }
}


create_assign <- function(map_fn, output_nm, obj, idx, is_accu, has_init, is_back, is_redu) {

  if(grepl("walk", map_fn)) {
    return(NULL)
  }

  paste0(output_nm,
         if(!is_redu) {
           paste0('[[', idx, if (is_accu && has_init && !is_back) '+1L', ']]')
         },
         ' <- ')
}


create_obj_names <- function(obj, output_nm, obj_nms, is_lmap, is_modify, is_walk, is_accu, has_init, is_back) {

  if (is.null(obj_nms) || is_lmap || is_modify || is_walk) {
    return(NULL)
  }

  nms <- paste0('names(', obj, ')')

  if (is_accu && is_back) {
    nms <- paste0('rev(', nms, ')')
    if (has_init) {
      nms <- paste0('c(', nms, ', ".init")')
    }
  } else if (is_accu && has_init) {
    nms <- paste0('c(".init", ', nms, ')')
  }

  paste0('\nnames(', output_nm, ') <- ', nms, '\n')
}


create_null_return <- function(maybe_assign, returns_null, is_redu, is_lmap, is_extr_fn, def) {

  has_def <- !is.null(def)

  def <- if (has_def && is.name(def)) {
    as.character(def)
    } else {
      deparse_expr(def)
    }

  if ((returns_null && !is_redu && !is_lmap) || is_extr_fn) {

    if_tmp <- 'if (!is.null(.tmp)) '

    if (!has_def) {

      return(paste0(if_tmp, maybe_assign, '.tmp\n'))

    } else if (is_extr_fn && has_def) {

      else_def <- paste0(' else ', def)
      return(paste0(maybe_assign, if_tmp, '.tmp\n', else_def))
    }
  }
  return(NULL)
}

create_if_selector <- function(obj, maybe_if, else_fn, is_lmap) {
  if (!is.null(maybe_if) && is.null(else_fn) && !is_lmap) {
    paste0('.sel <- vector("logical", length = length(', obj,'))\n')
  } else {
    NULL
  }
}

create_loop_start <- function(idx, obj, maybe_at, is_back, is_lmap, is_redu, is_accu, has_init) {
  paste0('\nfor (',idx,' in ',
          if (is_back) 'rev(',
         'seq_along(', obj, ')',
          if (is_back) ')',
          if (!is.null(maybe_at) && !is_lmap) '[.sel]',
          if ((is_redu || is_accu) && !has_init) '[-1]',
         ') {\n'
         )
}

create_at_nonselected <- function(maybe_at, is_lmap, output_nm, obj) {
  if (!is.null(maybe_at) && !is_lmap) paste0('\n', output_nm, '[-.sel] <- ', obj,'[-.sel]\n')
}

create_if_nonselected <- function(maybe_if, else_fn, is_lmap, output_nm, obj) {
  if (!is.null(maybe_if) && is.null(else_fn) && !is_lmap) paste0('\n', output_nm, '[.sel] <- ', obj,'[.sel]\n')
}

create_tmp_output <- function(maybe_assign, returns_null, is_redu, is_lmap, is_extr_fn) {
  if ((returns_null && !is_redu && !is_lmap) || is_extr_fn) {
    '.tmp <-'
  } else {
    maybe_assign
  }
}

create_lmap_stop <- function(is_lmap, output_nm, idx) {
  if (is_lmap) {
    paste0('stopifnot(is.list(', output_nm,'[[', idx, ']]))\n')
  } else {
    NULL
  }
}

create_warning <- function(yields_error) {
  if (is.null(yields_error)) {
    paste0("# --- WARNING: above call has not been checked --- #\n")
  } else if (yields_error) {
    paste0("# --- WARNING: error detected in the call above --- #\n")
  } else {
    NULL
  }
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

post_process <- function(obj, fn_env, output_nm, is_lmap, is_accu, is_accu2) {

  if(is_lmap) {

    flat <- paste0('flatten(', output_nm, ')')

    if (is.data.frame(eval(parse(text = obj), envir = fn_env))) {
      return(paste0('\n', output_nm, ' <- dplyr::as_tibble(', flat, ')\n'))
    } else {
      return(paste0('\n', output_nm, ' <- ', flat, '\n'))
    }

  } else if (is_accu && !is_accu2) {

    return(paste0('if (all(lengths(', output_nm, ') == 1L)) {\n',
                  output_nm, ' <- unlist(', output_nm, ')\n}',
                  '\n'))

  } else NULL

}


add_at <- function(map_fn, obj, output_nm, idx, at, fn_env) {

  if (is.null(at)) {
    return(NULL)
  }

  # FIXME: Problem if deparse is used
  is_char <- is.character(eval(at, envir = fn_env))
  if (!is_char && !is.numeric(eval(at, envir = fn_env))) {
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

add_if <- function(fn_str, obj, output_nm, idx, p_fn,
                   else_fn, brk, fn_env, cl_chr, var_nms) {

  if (is.null(p_fn)) {
    return(fn_str)
  }

    add_if <- rewrite_fn(fn_expr = p_fn,
                       .inp_objs = obj,
                       .idx = idx,
                       output_nm = output_nm,
                       var_nms = var_nms,
                       fn_env = fn_env,
                       cl_chr = cl_chr,
                       add_if = TRUE)

  add_else <- NULL

  if (!is.null(else_fn)) {

    add_else <- rewrite_fn(fn_expr = else_fn,
                           .inp_objs = obj,
                           .idx = idx,
                           output_nm = output_nm,
                           var_nms = var_nms,
                           fn_env = fn_env,
                           cl_chr = cl_chr,
                           add_else = TRUE)

  } else {
    add_else <- paste0(obj, brk$o, idx, brk$c)
  }

  return(paste0('if (', add_if, ') {\n',
                fn_str,'\n',
                '} else {\n',
                add_else, '\n',
                '}\n'))
}


# add_if <- function(map_fn, obj, output_nm, idx, p_fn, else_fn, brk, fn_env, cl_chr, var_nms) {
#
#   if (is.null(p_fn)) {
#     return(NULL)
#   }
#
#   fn_str <- rewrite_fn(fn_expr = p_fn,
#                        .inp_objs = obj,
#                        .idx = idx,
#                        output_nm = output_nm,
#                        var_nms = var_nms,
#                        fn_env = fn_env,
#                        cl_chr = cl_chr,
#                        add_if = TRUE)
#
#   add_else <- NULL
#
#   if (!is.null(else_fn)) {
#
#     add_else <- rewrite_fn(fn_expr = else_fn,
#                            .inp_objs = obj,
#                            .idx = idx,
#                            output_nm = output_nm,
#                            var_nms = var_nms,
#                            fn_env = fn_env,
#                            cl_chr = cl_chr,
#                            add_else = TRUE)
#
#     else_str <- paste0(output_nm, '[[', idx, ']] <- ', add_else, '\n')
#   } else {
#     else_str <- if (map_fn == "lmap_if") {
#       paste0(output_nm, '[[', idx, ']] <- ', obj, brk$o, idx, brk$c, '\n')
#     } else {
#       paste0('.sel[', idx,'] <- TRUE\n')
#     }
#   }
#
#   return(paste0('if (!(', fn_str, ')) {\n',
#                 else_str,
#                 'next\n', '}\n'))
# }


# create vector with names of variables used in for loop
create_var_nms <- function(has_at, has_p, has_tmp, bare_inp_nms, is_lmap, is_i, cl_chr, output_nm) {

  var_nms <- c(if (has_at && !is_lmap) ".at",
               if (has_p || has_at) ".sel",
               if (has_tmp) ".tmp",
               if (is_i) ".idx",
               bare_inp_nms)

  if (length(var_nms) != length(unique(var_nms))) {

    x <- var_nms[duplicated(var_nms)]

      rlang::abort(
        c("Problem with `as_loop()` input `.expr`.",
          x = paste0("Input objects in ", cl_chr, " must not share the same name with temporary variables in the resulting `for` loop."),
          i = paste0("The following input objects need to be renamed: ", paste(paste0('`', x, '`'), collapse = ", "), ".")
        )
      )
  }

  if (output_nm %in% var_nms) {

    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        x = paste0('A temporary variable in the resulting `for` loop shares the same name as the output object specified in `output_nm`: "', output_nm, '",'),
        i = paste0("Please rename `as_loop()`'s `output_nm` argument to a name which is not used as temporary variable inside the resulting `for` loop.")
      )
    )
  }

  var_nms

}


set_brackets <- function(is_lmap) {
  if (is_lmap) {
    brk <- list(o = '[',
                c = ']')
  } else {
    brk <- list(o = '[[',
                c = ']]')
  }
  brk
}
