# TODO: make work with no function in map => then just screen input (fix .print method)
# TODO: make work with more than one input (map2, pmap accu => ok, redu & redu2 & accu2)
# TODO: include `.p` and `.at`
#' @export
screen <- function(expr, res_fns = list(class), inp_fns = list(class), max_inp_no = 2) {

  check_pos_integer(max_inp_no, "screen", "max_inp_no")

  q         <- rlang::enquo(expr)
  q_res_fns <- rlang::enquo(res_fns)
  q_inp_fns <- rlang::enquo(inp_fns)

  if (!is.null(res_fns)) {
    if (!is.list(res_fns)) res_fns <- list(res_fns)
    map(res_fns, ~ check_functionable(.x, "screen", "res_fns"))
    res_fns <- transform_fn(res_fns, q_res_fns, "res_fns")
  }

  if (!is.null(inp_fns)) {
    if (!is.list(inp_fns)) res_fns <- list(inp_fns)
    map(inp_fns, ~ check_functionable(.x, "screen", "inp_fns"))
    inp_fns <- transform_fn(inp_fns, q_inp_fns, "inp_fns")
  }

  q <- unpipe_expr(q,
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

  is_accu_redu   <- grepl("^(accumulate|reduce)$", map_fn_chr, perl = TRUE)
  is_accu_redu2  <- grepl("^(accumulate|reduce)2$", map_fn_chr, perl = TRUE)
  has_init       <- !is.null(expr_ls[[".init"]])

  inp_objs       <- get_inp_objs(expr_ls, q_env, max_inp_no, is_accu_redu2, has_init)

  has_fn         <- !is.null(expr_ls[[".f"]])

  if (has_fn || is_accu_redu || is_accu_redu2) {

    # res <- wrap(..expr   = !! q,
    #             .f       = function(x) quietly(safely(x)),
    #             ..silent = TRUE)

    res <- wrap(..expr   = !! q,
                .f       = function(x) quietly2(safely2(x)),
                ..silent = TRUE)

    res <- restore_all(res)
    results_ls  <- map(res, c("result", "result"), .default = NULL)

    if (is_accu_redu) {
      if(!has_init) {
        inp_objs[["input_y"]] <- append(list(NULL), results_ls[-length(results_ls)])
      } else {
        init    <- eval(expr_ls[[".init"]], envir = q_env)
        inp_objs[["input_y"]] <- append(init, results_ls[-length(results_ls)])
      }
    }
  }

  if (has_fn) {

    errors_ls   <- map(res, c("result", "error"), .default = NA)
    output_ls   <- map(res, c("output"), .default = NA)
    warnings_ls <- map(res, c("warnings"), .default = NA)
    message_ls  <- map(res, c("messages"), .default = NA)

    errors_ls   <- map(errors_ls, .f = get_error_msg)

    init_res    <- tibble::tibble(inp_objs,
                                  result  = results_ls,
                                  error   = errors_ls,
                                  output  = output_ls,
                                  warning = warnings_ls,
                                  message = message_ls)

    # if redu_accu

    # if redu_accu2

  } else {
    init_res <- tibble::tibble(inp_objs)

    if (is.null(init_res)) {
      rlang::warn(c(i = paste0("`screen()` returning `NULL`, because:"),
                    paste0("\033[32m", "\u23F5", "\033[39m", " `max_inp_no` is set to `0` AND"),
                    paste0("\033[32m", "\u23F5", "\033[39m", " `expr` doesn't contain an `.f` argument.")
                    )
      )
      }
  }

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

  out2 <- dplyr::mutate(out,
                        dplyr::across(everything(),
                                      ~ unlist_if_all_length_one(.x, dplyr::cur_column())
                        )
  )

  attach_screen_attr(out2, cl_chr)
}

attach_screen_attr <- function(x, cl_chr) {

  ln <- nrow(x)

  attr(x, "class")       <- c("screen_tbl", attr(x, "class"))
  attr(x, "call")        <- cl_chr

  attr(x, "no_err")      <- no_err <- length(na.omit(x$error))
  attr(x, "perc_err")    <- round((no_err/ln) * 100, 0)

  attr(x, "no_warn")     <- no_warn <- length(na.omit(x$warning))
  attr(x, "perc_warn")   <- round((no_warn/ln) * 100, 0)

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
                   "no_of_classes", "class_output")

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
  cl_chr <- attr(x, "call")
  no_err  <- attr(x, "no_err")
  perc_err <- attr(x, "perc_err")
  no_warn <- attr(x, "no_warn")
  perc_warn <- attr(x, "perc_warn")
  # res_classes <-  attr(x, "res_classes")
  no_of_classes <-  attr(x, "no_of_classes")
  class_output <- attr(x, "class_output")

  cat(paste_subtle("# Screen call: ", cl_chr, "\n"))
  cat(paste_subtle("# No. of errors: ",   no_err,  " (", perc_err, "%)", " || "))
  cat(paste_subtle("# No. of warnings: ", no_warn, " (", perc_warn, "%)", "\n"))

  if (no_of_classes > 0) {
    cat(paste_subtle("# No. of result classes: ", no_of_classes), "\n")
    cat(paste_subtle("# └─ ", trimws(class_output, "right"), ".\n"))
  }
  NextMethod()
}

unlist_if_all_length_one <- function(x, col_nm) {

  if (!col_nm == "result" & !startsWith(col_nm, "input")) {
    x <- ifelse(lengths(x) == 0, NA, x)
  }

  if( all(lengths(x) == 1L)) {
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

as_listwraped_fn <- function(f) {
  purrr::compose(list, rlang::as_function(f))
}

name_if_symbol <- function(x, arg) {
  x1 <- x[[1]]
  if (is.symbol(x1)) {
    nm <- deparse(x1)
  } else if (is.character(x1)) {
    nm <- x1
  } else {
    rlang::abort(c(paste0("Problem with `screen()` input `", arg, "`."),
                   i = "Anonymous functions in `", arg, "` must be named.",
                   x = "Unnamed function in `", arg, "` detected.")
                 )
  }
  list(nm)
}

transform_fn <- function(fns, q_fns, arg) {
  expr_ls    <- rlang::quo_get_expr(q_fns)
  fns_nm_ls  <- lmap(expr_ls[-1], ~ name_if_symbol(.x, arg))
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

    out <- tibble::new_tibble(list(input_x = out), nrow = length(out))
    return(out)
  }

  if (!is.null(expr_ls[[".y"]]) && max_inp_no >= 2) {

    inp_y    <- eval(expr_ls[[".y"]], envir = q_env)
    tmp      <- list(inp_x, inp_y)
    named_xy <- set_names(tmp, paste0("input_", c("x", "y")))
    out      <- tibble::new_tibble(named_xy, nrow = length(inp_x))
    return(out)
  }

  if (!is.null(expr_ls[[".l"]]) && max_inp_no > 0) {

    tmp      <- eval(expr_ls[[".l"]], envir = q_env)
    inp_l    <- tmp[seq_len(max_inp_no)]
    named_l  <- set_names(inp_l, paste0("input_l", seq_len(max_inp_no)))
    out      <- tibble::new_tibble(named_l, nrow = length(inp_l[[1]]))
    return(out)
  }

  out <- tibble::new_tibble(list(input_x = out), nrow = length(out))

}
