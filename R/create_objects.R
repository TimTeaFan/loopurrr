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


create_out_obj <- function(map_fn, obj, output_nm, has_init, init) {

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

  if (grepl("^accumulate$", map_fn)) {
    mde <- paste0("mode(", obj, ")")
    lng <- paste0("length(", obj, ")", if (has_init) "+1L")
  } else {
    mde <- paste0('"', mde ,'"')
    lng <- paste0("length(", obj, ")")
  }

  if (grepl("reduce", map_fn)) {
    vec <- if (has_init) {
      if (is.name(init)) {
        as.character(init)
      } else {
        deparse_expr(init)
      }
    } else {
      obj
    }
    return(paste0(output_nm, ' <- ', vec, '[[1]]', '\n'))
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
# add_selection_old <- function(map_fn, obj, obj_nms, output_nm, idx, fn_env, at = NULL, p_fn = NULL, else_fn = NULL) {
#
#   # map_at
#   if (grepl("^l{0,1}map_at", map_fn, perl = TRUE) && !is.null(at)) {
#
#     # FIXME: Problem if deparse is used:
#     if (is.language(at) && !is.name(at)) {
#       at <- eval(at)
#     }
#     # is name
#     if (is.name(at)) {
#
#       at_obj <- get(at, envir = fn_env)
#
#       if (is.character(at_obj)) {
#
#         if (map_fn == "map_at") {
#           return(paste0('.sel <- which(names(', obj,') %in% ', at,')\n'))
#         } else {
#           return(paste0('.sel <- names(', obj,') %in% ', at,')\n'))
#         }
#
#       } else {
#         if (map_fn == "map_at") {
#           return(paste0('.sel <- ', at, '\n'))
#         } else {
#           return(paste0('.sel <- seq_along(', obj, ') %in% ', at, '\n'))
#         }
#       }
#       # is character
#     } else if (is.character(at)) {
#       stopifnot(!is.null(obj_nms))
#
#       sel <- obj_nms %in% at
#
#       if (map_fn == "map_at") {
#         return(
#           paste0('.at <- c(',
#                  paste(paste0('"', at, '"'), collapse = ", "),
#                  ')\n',
#                  '.sel <- which(names(', obj,') %in% .at)\n')
#         )} else {
#           return(paste0('.sel <- names(', obj,') %in% ',
#                         'c(',
#                         paste(paste0('"', at, '"'), collapse = ", "),
#                         ')\n'))
#         }
#       # is numeric
#     } else {
#       if (map_fn == "map_at") {
#         return(paste0('.sel <- c(',
#                       paste0(at, collapse = ","),
#                       ')\n'))
#       } else {
#         return(paste0('.sel <- seq_along(', obj, ') %in% ',
#                       'c(',
#                       paste0(at, collapse = ","),
#                       ')\n'))
#       }
#     }
#     # map_if
#   } else if (grepl("^l{0,1}map_if", map_fn, perl = TRUE) && !is.null(p_fn)) {
#     fn_str <- rewrite_fn(p_fn, obj, idx)
#     add_else <- NULL
#
#     if(!is.null(else_fn)) {
#       add_else <- rewrite_fn(else_fn, obj, idx)
#       else_str <- paste0(output_nm, '[[', idx, ']] <- ', add_else, '\n')
#     } else {
#       else_str <- paste0('.sel[', idx,'] <- TRUE\n')
#     }
#
#     return(paste0('if (!', fn_str, ') {\n',
#                   else_str,
#                   'next\n', '}\n'))
#   }
# }

