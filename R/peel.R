peel <- function(expr, i = 1L, args = c("all", ".x", ".y", ".l"), simplify = TRUE) {

  args <- match.arg(args, c("all", ".x", ".y", ".l"), several.ok = TRUE)

  if ("all" %in% args) {
    args <- c(".x", ".y", ".l")
  }

  q <- rlang::enquo(expr)

  # check and unpipe
  q <- unpipe_expr(q,
                   sc = sys.calls(),
                   is_dot = match.call()$expr == ".",
                   calling_fn = "peel")

  q_expr        <- rlang::quo_get_expr(q)
  q_env         <- rlang::quo_get_env(q)

  map_fn_chr    <- deparse_and_rm_nmspace(q_expr[[1]])
  map_fn        <- mabye_check_map_fn(map_fn_chr, "peel", checks = TRUE)
  expr_ls       <- reformat_expr_ls(q_expr = q_expr, fn = map_fn)

  is_imap       <- grepl("(^imap)|(^iwalk)|(^imodify)", map_fn_chr, perl = TRUE)
  is_lmap       <- grepl("^lmap", map_fn_chr, perl = TRUE)
  is_walk       <- grepl("^(walk|iwalk|pwalk)", map_fn_chr, perl = TRUE)
  is_redu       <- grepl("reduce", map_fn_chr, perl = TRUE)
  is_accu       <- grepl("^accumulate", map_fn_chr, perl = TRUE)
  is_accu_redu2 <- grepl("^(accumulate|reduce)2", map_fn_chr, perl = TRUE)
  is_back       <- !is.null(expr_ls[[".dir"]]) && expr_ls[[".dir"]] == "backward"

  has_init      <- !is.null(expr_ls[[".init"]])

  # get all objects .x, .y or .l
  obj_ls   <- get_all_objs(expr_ls, args, q_env, is_imap)
  main_obj <- obj_ls[[".x"]] %||% obj_ls[[".l"]][[1]]
  fst_obj  <- main_obj %||% obj_ls[[".y"]]

  # preserve original i
  old_i <- i

  # invert i if necessary
  if ((is_redu || is_accu) && is_back) {
    i <- rev(i)
  }

  # perform checks and convert, adjust i if necessary
  i <- check_and_convert_i(i = i,
                           main_obj = main_obj,
                           obj = fst_obj,
                           calling_fn = "peel",
                           is_redu = is_redu,
                           is_accu = is_accu,
                           has_init = has_init,
                           is_back = is_back)

  out <- subset_elements(obj_ls = obj_ls,
                         expr_ls = expr_ls, q_env = q_env,
                         q = q, i = i, old_i, is_lmap = is_lmap, is_redu = is_redu,
                         is_accu = is_accu, is_accu_redu2 = is_accu_redu2,
                         has_init = has_init, is_back = is_back)


  if (length(out) == 1L) {
    names(out) <- NULL
  }

  # maybe simplify
  if (length(out) == 1L) {
    out <- flatten(out)
  } else if (is_redu || is_accu) {

  } else {
    out <- transpose(out)
  }

  if (simplify && is.list(out) && length(out) == 1L && !is_lmap) {
    out <- unlist(out, recursive = FALSE)
  } else if (simplify && is.list(out) && all(lengths(out) == 1L)) {
    out <- flatten(map(out, unname))
  }

  out

}

# ------------------------------ #
# peel()'s helper functions ----
# ------------------------------ #

# get the object of map or pmap call's
get_all_objs <- function(expr_ls, args, q_env, is_imap) {

  if (is_imap) {
    expr_ls$.y <- bquote(names(.(expr_ls[[".x"]])) %||% seq_along(.(expr_ls[[".x"]])))
  }

  obj_ls <- map(set_names(args), ~ eval(expr_ls[[.x]], envir = q_env))

  out <- compact(obj_ls)

  out

}

subset_elements <- function(obj_ls, expr_ls, q_env, q, i, old_i, is_lmap, is_redu, is_accu,
                            is_accu_redu2, has_init, is_back) {

  if ((is_redu || is_accu) && max(old_i) == 1) {

    obj_x_len <- length(obj_ls[[".x"]])

    out_x <- if (has_init) {
      eval(expr_ls[[".init"]], envir = q_env)
    } else {
      if (is_back) {
        obj_ls[[".x"]][[obj_x_len]]
      } else {
        obj_ls[[".x"]][[1]]
      }
    }


    out_y <- if (is_back) {
      obj_ls[[".x"]][[obj_x_len - !has_init]]
    } else {
      obj_ls[[".x"]][[2 - has_init]]
  }

    return(list(.x = out_x,
                .y = out_y))
  }

  out <- imap(obj_ls,
       ~ subset_elements_imp(el = .x,
                             nm = .y,
                             i = i,
                             is_lmap = is_lmap,
                             is_redu = is_redu,
                             is_accu = is_accu,
                             is_accu_redu2 = is_accu_redu2,
                             has_init = has_init,
                             is_back = is_back)
  )

  if (is_redu || is_accu) {
    expr_ls[[1]] <- if (is_accu_redu2) rlang::expr(accumulate2) else rlang::expr(accumulate)
    if(!is.null(out[[".y"]])) {
      out[[".z"]] <- out[[".y"]]
    }
    out[[".y"]] <- if (is_back) rev(out[[".x"]]) else out[[".x"]]

    if (is_back) {
      obj_x_len <- length(obj_ls[[".x"]])
      i <- obj_x_len - rev(i) + 1
    }

    peek_i <- i[-length(i)]

    q <- rlang::quo_set_expr(q, rlang::expr(peek(!! as.call(expr_ls), i = !! peek_i)))
    peek_out <- rlang::eval_tidy(q)

    out[[".x"]] <- append(out[[".y"]][[1]], peek_out)

    if (has_init) {
      out[[".x"]][1] <- eval(expr_ls[[".init"]], envir = q_env)
    } else {
      out[[".y"]] <- out[[".y"]][-1]
    }

    out <- transpose(out)
    out <- out[old_i]
  }

  out
}

subset_elements_imp <- function(el, nm, i, is_lmap, is_redu, is_accu, is_accu_redu2, has_init, is_back) {

  # if object is `.l`
  if (nm == ".l") {
    out <- map(el, ~ `[`(.x, i))

    if (rlang::is_named(out)) {
      names(out) <- NULL
    }

    if (any(map_lgl(out, rlang::is_named))) {
      out <- modify(out, unname)
    }

    if (length(i) > 1) {
      out <- transpose(out)
    }

    return(out)
  }

  # if accu or redu adjust `i`
  if ((is_redu || is_accu) && !has_init) {
    if (is_back) {
      i <- append(min(i) - 1, i)
    } else {
      i <- append(i, max(i) + 1)
    }
  }

  # if accu or redu adjust `i`
  if (nm == ".y" && is_accu_redu2 && !has_init) {
    i <- i - 1L
  }

  # for all other cases just subset `el` by `i` and unname
  out <- `[`(el, i)

  if (!is_lmap && rlang::is_named(out)) {
    names(out) <- NULL
  }

  out

}
