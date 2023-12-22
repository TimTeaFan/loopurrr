# TODO:
# add `f` column to result data.frame to show where f or else was exectuded

#' @export
screen <- function(expr,
                   res_fns = list(class),
                   inp_fns = list(class),
                   max_inp_no = 2) {

  # checks and setup
  check_pos_integer(max_inp_no, "screen", "max_inp_no")

  q              <- rlang::enquo(expr)
  q_res_fns      <- rlang::enquo(res_fns)
  q_inp_fns      <- rlang::enquo(inp_fns)

  res_fns        <- check_and_maybe_transform(res_fns, q_res_fns, "res_fns")

  inp_fns        <- check_and_maybe_transform(inp_fns, q_inp_fns, "inp_fns")

  q              <- unpipe_expr(q,
                                sc = sys.calls(),
                                is_dot = match.call()$`expr` == ".",
                                calling_fn = "screen")

  q_expr         <- rlang::quo_get_expr(q)
  q_env          <- rlang::quo_get_env(q)

  map_fn_chr     <- deparse_and_rm_nmspace(q_expr[[1]])

  map_fn         <- mabye_check_map_fn(map_fn_chr, "screen", checks = TRUE)

  q_ex_std       <- match.call(definition = map_fn, call = q_expr)

  cl_chr         <- call_as_chr(q_expr)

  expr_ls        <- reformat_expr_ls(q_expr = q_expr, fn = map_fn)
  at_idx         <- eval(expr_ls[[".at"]], envir = q_env)
  p_fn           <- eval_as_fn(expr_ls[[".p"]], env = q_env)
  else_fn        <- eval_as_fn(expr_ls[[".else"]], env = q_env)
  init           <- eval(expr_ls[[".init"]], envir = q_env)

  is_accu_redu   <- grepl("^(accumulate|reduce)", map_fn_chr, perl = TRUE)
  is_accu_redu2  <- grepl("^(accumulate|reduce)2$", map_fn_chr, perl = TRUE)
  is_back        <- !is.null(expr_ls[[".dir"]]) && expr_ls[[".dir"]] == "backward"
  is_redu        <- grepl("^reduce", map_fn_chr, perl = TRUE)
  has_init       <- !is.null(expr_ls[[".init"]])
  is_typed       <- check_if_typed(map_fn_chr)

  inp_objs       <- get_inp_objs(expr_ls, q_env, max_inp_no, is_accu_redu2, has_init)
  has_fn         <- !is.null(expr_ls[[".f"]])
  has_at_or_p    <- !is.null(expr_ls[[".at"]]) || !is.null(expr_ls[[".p"]])

  q              <- adjust_quo_if_reduce(q, expr_ls, is_redu)

  use_env        <- is_accu_redu | is_typed

  # actual function:
  # if map call has a function
  if (has_fn) {

    maybe_prepare_capture_env(inp_objs[[1]], is_accu_redu, has_init, use_env)

    res          <- wrap_and_try(q, use_env, is_typed, map_fn_chr)

    res          <- restore(res, has_init, is_back, env = is_accu_redu)

    # if functionless map call
  } else {

    warn_inform_return(max_inp_no, is_accu_redu, cl_chr)
    res <- NULL

  }

  init_res       <- finalize_inp_obj(inp_objs, res, is_accu_redu, has_init, is_back, init, q_env)

  out            <- finalize_output(init_res, res, inp_fns, res_fns, has_fn, cl_chr)

  out

}

attach_screen_attr <- function(x, cl_chr, has_fn) {

  ln <- nrow(x)

  attr(x, "class")       <- c("screen_tbl", attr(x, "class"))
  attr(x, "call")        <- cl_chr
  attr(x, "has_fn")      <- has_fn

  if (has_fn) {
    attr(x, "no_err")      <- no_err <- length(na.omit(x[["error"]]))
    attr(x, "perc_err")    <- round((no_err/ln) * 100, 0)

    attr(x, "no_warn")     <- no_warn <- length(na.omit(x[["warning"]]))
    attr(x, "perc_warn")   <- round((no_warn/ln) * 100, 0)
  }

  if (!is.null(x[["result_class"]])) {
    res_classes          <- na.omit(unique(x[["result_class"]][is.na(x[["error"]])]))
  } else {
    res_classes          <- NULL
  }

  attr(x, "no_of_classes") <- no_of_classes <- length(res_classes)

  if ( no_of_classes > 0) {

    class_res    <- purrr::imap(res_classes, ~ paste0("[", .y, "] ", paste(.x, collapse = ", ")))
    class_output <- paste(class_res, collapse = ", ")

    if (nchar(class_output) > 72) {
      class_output <- paste0(substr(class_output, 1, 75), " ...")
    }

    attr(x, "class_output") <- class_output
  }
  x
}

#' @export
summary.screen_tbl <- function(x) {

  screen_attr <- c("call", "no_err", "perc_err", "no_warn", "perc_warn",
                   "no_of_classes", "class_output", "has_fn")

  attr_x <- attributes(x)[screen_attr]

  idx_dat <- dplyr::mutate(x, idx = dplyr::row_number(), .before = 1L)
  grp_dat <- dplyr::group_by(idx_dat, dplyr::pick(!c(input, result)))

  res <- dplyr::summarise(grp_dat,
                          idx_ls = list(idx),
                          idx    = paste(idx, collapse = ", "),
                          .groups = "drop"
                          )

  res <- dplyr::mutate(res,
                       type = dplyr::case_when(
                         is.na(error) & is.na(warning) ~ "result",
                         !is.na(error)                 ~ "error",
                         !is.na(warning)               ~ "warning"
                       ),
                       .before = 1L)

  res <- dplyr::ungroup(res)

  nms <- make.names(res$type, unique = TRUE)
  res$idx_ls <- set_names(res$idx_ls, nms)

  attributes(res) <- c(attributes(res), attr_x)
  class(res) <- c("screen_tbl", class(res))

  res
}

#' @export
print.screen_tbl <- function(x) {
  cl_chr        <- attr(x, "call")
  no_err        <- attr(x, "no_err")
  perc_err      <- attr(x, "perc_err")
  no_warn       <- attr(x, "no_warn")
  perc_warn     <- attr(x, "perc_warn")
  no_of_classes <- attr(x, "no_of_classes")
  class_output  <- attr(x, "class_output")
  has_fn        <- attr(x, "has_fn")

  cat(paste_subtle("# Screen call: ", cl_chr, "\n"))
  if(!has_fn) {
    cat(paste_subtle("# Call doesn't contain function in `.f`. Screening input:\n"))
  }
  if (!is.null(no_err)) {
    cat(paste_subtle("# No. of errors: ",   no_err,  " (", perc_err, "%)", " || "))
  }

  if (!is.null(no_warn)){
    cat(paste_subtle("# No. of warnings: ", no_warn, " (", perc_warn, "%)", "\n"))
  }

  if (no_of_classes > 0) {
    cat(paste_subtle("# No. of result classes: ", no_of_classes), "\n")
    cat(paste_subtle("# └─ ", trimws(class_output, "right"), ".\n"))
  }
  NextMethod()
}

unlist_if_all_length_one <- function(x, col_nm) {

  if (!col_nm %in% c("result", "error")) {
    x <- ifelse(lengths(x) == 0, NA, x)
  }

  if (all(lengths(x) == 1L)) {
    unlist(x, recursive = FALSE)
  } else {
    x
  }
}

get_error_msg <- function(x) {
  tryCatch(conditionMessage(x),
           error = function(e) {
             NA
           }
  )
}

paste_subtle <- function(...) {
  pillar::style_subtle(paste0(...))
}

name_if_symbol <- function(x, arg) {

  nm <- names(x)

  if (is.null(nm)) {
    x1 <- x[[1]]
    if (is.symbol(x1)) {
      nm <- deparse(x1)
    } else if (is.character(x1)) {
      nm <- x1
    } else {
      rlang::abort(c(paste0("Problem with `screen()` input `", arg, "`."),
                     i = paste0("Anonymous functions in `", arg, "` must be named."),
                     x = paste0("Unnamed function in `", arg, "` detected.")
                     )
      )
    }
  }

  list(nm)
}

transform_fn <- function(fns, q_fns, arg) {
  expr_ls    <- rlang::quo_get_expr(q_fns)
  fns_nm_ls  <- lmap(as.list(expr_ls)[-1], ~ name_if_symbol(.x, arg))
  new_fns    <- set_names(fns, fns_nm_ls)

  map(new_fns, as_listwraped_fn)
}

as_listwraped_fn <- function(f) {
  purrr::compose(list, rlang::as_function(f))
}

get_inp_objs <- function(expr_ls, q_env, max_inp_no, is_accu_redu2, has_init) {

  if (max_inp_no == 0) {
    return(NULL)
  }

  out <- eval(expr_ls[[".x"]], envir = q_env)

  if (!is.null(expr_ls[[".x"]]) && max_inp_no == 1) {

    out <- list(input_x = out)
    return(out)
  }

  if (!is.null(expr_ls[[".y"]]) && max_inp_no >= 2) {

    inp_y    <- eval(expr_ls[[".y"]], envir = q_env)
    tmp      <- list(out, inp_y)
    named_xy <- set_names(tmp, paste0("input_", c("x", "y")))

    return(named_xy)
  }

  if (!is.null(expr_ls[[".l"]]) && max_inp_no > 0) {

    tmp      <- eval(expr_ls[[".l"]], envir = q_env)
    inp_l    <- tmp[seq_len(max_inp_no)]
    named_l  <- set_names(inp_l, paste0("input_l", seq_len(max_inp_no)))

    return(named_l)
  }

  out <- list(input_x = out)

}

# accounts for accumulate and reduce and turns input list into tibble
finalize_inp_obj <- function(inp_objs, res, is_accu_redu, has_init, is_back, init, q_env) {

  if (is_accu_redu && !is.null(res)) {
    inp_objs <- as.list(inp_objs)

    if (has_init) {

      if (is_back) {
        inp_objs[["input_y"]] <- append(inp_objs[["input_x"]], init)
      } else {
        inp_objs[["input_y"]] <- append(init, inp_objs[["input_x"]])
      }

      } else {
        inp_objs[["input_y"]] <- inp_objs[["input_x"]]
      }

    if (is_back) {
      inp_objs[["input_x"]] <- append(res$result[-1], list(NULL))
    } else {
      inp_objs[["input_x"]] <- append(list(NULL), res$result[-nrow(res)])
    }

  }

  out <- tibble::new_tibble(inp_objs,
                            nrow = length(inp_objs[[1]]))

  dplyr::mutate(out, i = add_i(is_back, is_accu_redu), .before = 1L)
}

adjust_quo_if_reduce <- function(q, expr_ls, is_redu) {

  if (is_redu) {
    expr_ls      <- reduce2accumulate(expr_ls)
    q            <- rlang::quo_set_expr(q, as.call(expr_ls))
  }

  q
}

warn_inform_return <- function(max_inp_no, is_accu_redu, cl_chr) {

  if (max_inp_no == 0) {

    rlang::warn(c(i = paste0("`screen()` returning `NULL`, because:"),
                  paste0("\033[32m", "\u23F5", "\033[39m", " `max_inp_no` is set to `0` AND"),
                  paste0("\033[32m", "\u23F5", "\033[39m", " `expr` doesn't contain a function in `.f`.")
                  )
                )

  } else {

    if (is_accu_redu) {
      msg <- "Returning `.x` input only:"
    } else {
      msg <- "Returning only inputs:"
    }

    rlang::inform(c(paste0("Screening call: ", cl_chr),
                    i = "`.f` argument not supplied.",
                    i = msg))
  }

}

finalize_output <- function(init_res, res, inp_fns, res_fns, has_fn, cl_chr) {

  init_res <- tibble::tibble(init_res, res)

  out <- dplyr::rowwise(init_res)

  if (!is.null(inp_fns)) {
    out <- dplyr::mutate(out,
                         dplyr::across(tidyselect::starts_with("input"), inp_fns))

    if (has_fn) {
      out <- dplyr::relocate(out, starts_with("input"), .before = result)
    }

  }

  if (has_fn && !is.null(res_fns)) {
    out <- dplyr::mutate(out,
                         dplyr::across(tidyselect::starts_with("result"), res_fns),
                         .before = error)
  }

  out <- dplyr::ungroup(out)

  out <- dplyr::mutate(out,
                       dplyr::across(! matches("^input_[x,y]$") &
                                     ! matches("^input_l\\d+$") &
                                     ! matches("^result$"),
                                     ~ unlist_if_all_length_one(.x, dplyr::cur_column())
                        )
  )
  attach_screen_attr(out, cl_chr, has_fn)
}

check_and_maybe_transform <- function(fns, fns_quo, arg_nm) {

  if (!is.null(fns)) {
    check_list(fns, "screen", arg_nm)
    map(fns, ~ check_functionable(.x, "screen", arg_nm))
    fns <- transform_fn(fns, fns_quo, arg_nm)
  }

  fns
}

check_if_typed <- function(map_fn_chr){

  get_output_type(map_fn_chr) %in% c("dbl", "int", "lgl", "raw", "chr")

}

wrap_and_try <- function(q, use_env, is_typed, map_fn_chr) {

  res <- tryCatch({
    wrap(..expr    = !! q,
       .f        = function(x) capture(x, env = use_env),
       .else     = function(x) quietly(safely(x)),
       ..silent  = TRUE)
    }, error = function(e) {
      e
    })

  if (rlang::is_error(res)) {

    init_msg <- c(
      "Problem with `screen()` input `expr`.",
      i = "`screen()` only captures errors thrown by the functions supplied to `.f`.",
      x = paste0("An error in the setup of the overarching call to `", map_fn_chr, "` has been detected.")
      )

    if (is_typed) {
      unytped_map_fn <- gsub("_.*$", "", map_fn_chr)

      add_msg <- c(i = paste0("Try using the untyped version of your call: `",
                              unytped_map_fn , "`."))

    } else {
      add_msg <- c(i = paste0(
        "Please check if you correctly supplied the following arguments to `", map_fn_chr, "`:"),
        paste0("\033[32m", "\u23F5", "\033[39m", " a vector object to `.x`"),
        paste0("\033[32m", "\u23F5", "\033[39m", " a function (or string, integer or list) to `.f`")
        )
    }
    final_msg <- append(init_msg, add_msg)

    rlang::abort(final_msg)
  }

  res

}

maybe_prepare_capture_env <- function(inp_objs, is_accu_redu, has_init, use_env) {

  if (use_env) {
    no_i       <- infer_possible_i(inp_objs, is_accu_redu, has_init)
    prepare_capture_env(no_i)
  }

}
