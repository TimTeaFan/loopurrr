#' Show a list of supported function names
#'
#' @description
#' `get_supported_fns()` shows which functions are supported for a specific `{loopurrr}` function.
#' Currently, only works on `as_loop()`.
#'
#' @param fn The name of a `{loopurrr}` function as string.
#'
#' @returns
#' A list of supported function names as named character vectors.
#'
#' @section Examples:
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = TRUE}
#' options(width = 60)
#' get_supported_fns("as_loop")
#' ```
#'
#' @export
get_supported_fns <- function(fn) {

  all_basic_iterators <- list(

    map = c("map", "map_at", "map_chr", "map_dbl", "map_df", "map_dfc", "map_dfr", "map_if",
            "map_int", "map_lgl", "map_raw"),

    imap = c("imap", "imap_chr", "imap_dbl", "imap_dfc", "imap_dfr", "imap_int", "imap_lgl",
             "imap_raw"),

    map2 = c("map2", "map2_chr", "map2_dbl", "map2_df", "map2_dfc", "map2_dfr", "map2_int",
             "map2_lgl",  "map2_raw"),

    pmap = c("pmap", "pmap_chr", "pmap_dbl", "pmap_df", "pmap_dfc", "pmap_dfr", "pmap_int",
             "pmap_lgl", "pmap_raw"),

    lmap = c("lmap", "lmap_at"),

    modify = c("modify", "modify_at", "modify_if", "modify2", "imodify"),

    walk = c("iwalk", "pwalk", "walk", "walk2"),

    accumulate = c("accumulate", "accumulate2"),

    reduce = c("reduce", "reduce2")
  )

  supported_fns <- list(

    as_loop = all_basic_iterators,

    peek = all_basic_iterators,

    peel = all_basic_iterators,

    probe = all_basic_iterators,

    screen = all_basic_iterators
  )

  if (is.null(supported_fns[[fn]])) {
    rlang::inform("No supported functions. Did you specify the function name correctly?")
  } else {
    supported_fns[[fn]]
  }
}


deparse_expr <- function(call) {
  deparse(call,
          width.cutoff = 500L,
          backtick = TRUE,
          nlines = 1L,
          control = NULL)
}

call2chr <- function(expr) {
  dep_cl <- deparse(expr, width.cutoff = 70L)
  if (length(dep_cl) > 1 ) {
    del_cl <- paste0(dep_cl, collapse = "\n")
  }
  dep_cl
}

call_as_chr <- function(expr) {

  cl <- paste(trimws(deparse(expr)), collapse = " ")
  cl_ticks <- paste0("`", cl, "`")

  if (nchar(cl_ticks) > 50) {
    cl_ticks <- paste0(substr(cl_ticks, 1, 46), '...`')
  }

  cl_ticks
}

try_purr_call <- function(x, map_fn, map_fn_chr, par_frame) {

  # tmp <- rlang::eval_tidy(x)
  # map(tmp, )

  tryCatch({
    sink(nullfile())
    on.exit(sink(), add = TRUE)
    tmp <- rlang::eval_tidy(x)
    tmp
  }, error = function(e) {
      structure(paste0("map", " error"), class = "purrr-error")
  })
}


insert_and_reformat_text <- function(x) {

  before <- rstudioapi::getActiveDocumentContext()

  if (before$id == "#console") {
    rstudioapi::insertText(text = x)
  } else {
    rng_bfr <- before$selection[[1]]$range$start

    x_ln <- line_length(x)
    loc <- calc_location(before, x_ln)

    rstudioapi::insertText(location = c(loc[[1]], 1),
                           text = paste0(x, "\n\n"))

    after <- rstudioapi::getActiveDocumentContext()
    rng_aft <- if (is.null(loc[[2]])) {
      after$selection[[1]]$range$end
    } else {
      rstudioapi::as.document_position(c(loc[[2]], 1))
    }
    rng <- rstudioapi::document_range(c(loc[[1]], 1), rng_aft)
    if (after$id != "#console") {
      rstudioapi::setSelectionRanges(rng)
      rstudioapi::executeCommand('reformatCode')
    }
  }
}


line_length <- function(x) {
  length(unlist(strsplit(x, "\n")))
}

calc_location <- function(context, x_ln) {
  start <- context$selection[[1]]$range$start
  end   <- context$selection[[1]]$range$end

  loc <- if (identical(start, end) && start[2] == 1L) {
    out <- calc_last_line(context, start)
    list(out, NULL)
  } else if (end[2] == 1L) {
    return(list(end[1], NULL))
  } else {
    list(end[1] + 1L, end[1] + 1L + x_ln)
  }

  loc
}

calc_last_line <- function(context, loc) {

  con <- context$contents
  con_df <- dplyr::tibble(line = seq_along(con), code = con)

  following_lines <- con_df %>%
    dplyr::filter(dplyr::row_number() > loc[1]) %>%
    dplyr::pull(code)

  only_empty_lines <- !as.logical(sum(nchar(following_lines)))

  last_filled_line <- if(only_empty_lines) {
    loc[1] - 1
  } else {
    con_df %>%
      dplyr::filter(nzchar(gsub("\\s+", "", code))) %>%
      dplyr::filter(dplyr::lead(line) == loc[1]) %>%
      dplyr::pull(line)
  }
  last_filled_line + 1L
}


is_supported <- function(map_fn, loopurrr_fn, silent = FALSE) {

  supported_fns <- unlist(get_supported_fns(loopurrr_fn))

  not_supported_fns <- c("apply", "lapply", "vapply", "sapply", "rapply", "Map", "mapply", "tapply")

  if (map_fn %in% not_supported_fns) {
    if (!silent) {
    rlang::abort(
      c(paste0("Problem with `", loopurrr_fn, "` input `.expr`."),
        i = paste0("Currently `", loopurrr_fn, "` doesn't support functions from base R's apply family."),
        x = paste0("`", map_fn, "` is a function from base R's apply family."),
        i = paste0('For an overview of all currently supported {purrr} functions see the documentation `?', loopurrr_fn, '` or ',
                   'run `get_supported_fns("', loopurrr_fn, '")`')
        )
    )
    } else {
      FALSE
    }
  }

  if (!any(purrr::map_lgl(findFunction(map_fn), ~ rlang::env_name(.x) == "package:purrr"))) {
    if (!silent) {
      rlang::abort(
        c(paste0("Problem with `", loopurrr_fn, "` input `.expr`."),
          i = paste0("`", loopurrr_fn, "` only works with `map` and similar functions from the purrr package."),
          x = paste0("`", map_fn, "` is not located in the namespace of `package:purrr`."),
          i = paste0('For an overview of all currently supported {purrr} functions see the documentation `?', loopurrr_fn, '` or ',
                     'run `get_supported_fns("', loopurrr_fn, '")`')
          )
      )
    } else {
      FALSE
    }
  }

  if (!map_fn %in% supported_fns) {
    if (!silent) {
      rlang::abort(
        c(paste0("Problem with `", loopurrr_fn, "` input `.expr`."),
          i = paste0("Currently `", loopurrr_fn ,"` does only support certain {purrr} functions."),
          x = paste0("`", map_fn, "` is not supported yet."),
          i = paste0('For an overview of all currently supported {purrr} functions see the documentation `?', loopurrr_fn, '` or ',
                     'run `get_supported_fns("', loopurrr_fn, '")`')
          )
      )
    } else {
      FALSE
    }
  }
  map_fn %in% supported_fns
}

# adapted from https://stackoverflow.com/a/57417926/9349302
check_syntactical_nm <- function(x) {
  grepl("(^(\\p{L}|(\\.(\\p{L}|\\.|\\_)))(\\d|\\p{L}|\\.|\\_)*$)|(^\\.$)", x, perl = TRUE)
}

check_extr_fn <- function(fn_expr, e) {
  fn <- eval(fn_expr, envir = e)
  check <- is.numeric(fn) || is.character(fn)
  check
}


# This function checks if a function maybe a promise
# when applied to the output elements of `map` a promise might be hidden
# in another structure, in this case it won't be detected
#' @importFrom codetools findGlobals
check_lazy <- function(x, q_env) {
  out <- FALSE
  # if function: check that all global vars are accessible (if not: maybe lazy)
  if (is.function(x)) {
    global_vars <- codetools::findGlobals(x)
    res <- try(mget(global_vars, envir = q_env, inherits = TRUE), silent = TRUE)
    out <- if (inherits(res, "try-error")) TRUE
  }
  # if ggplot: check that mappings can be found in data (if not: maybe lazy)
  if (inherits(x, "ggplot")) {
    data_e <- as.environment(x$data)
    out <- any(purrr::map_lgl(x$mapping, ~ !exists(deparse(rlang::quo_get_expr(.x)), data_e)))
  }
  out
}

# check map_fn depending on arguments
mabye_check_map_fn <- function(fn, calling_fn, checks) {
  if (is_supported(fn, calling_fn, silent = !checks)) {
    fn <- get(fn, envir = rlang::as_environment("purrr"))
  } else {
    fn <- NULL
  }
  fn
}

#
# get_main_obj <- function(q_expr, q_env, map_fn_chr) {
#   expr_ls <- reformat_expr_ls(q_expr = q_expr, fn = map_fn)
# }

# deparse and remove namespace
deparse_and_rm_nmspace <- function(x) {

  map_fn_chr <- deparse(x)

  # remove namespace
  if (grepl("^\\w+::", map_fn_chr, perl = TRUE)) {
    map_fn_chr <- gsub("^\\w+::", "", map_fn_chr)
  }
  map_fn_chr
}


reformat_expr_ls <- function(q_expr, fn) {

  if (is.null(fn)) {
    return(as.list(q_expr[-1]))
  } else {
    q_ex_std <- match.call(definition = fn, call = q_expr)
    return(as.list(q_ex_std))
  }

}

extract_has_args <- function(returns_null, is_redu, is_lmap, is_extr_fn) {

  var_nms <- c("at"   = "at_idx",
               "p"    = "p_fn",
               "else" = "else_fn",
               "init" = "init")

  null_ls <- rep(list(NULL), length(var_nms))

  ifnotfound_ls <- purrr::set_names(null_ls,
                                    var_nms)

  has <- mget(var_nms,
              ifnotfound = ifnotfound_ls,
              envir = parent.frame())

  has <- purrr::map(var_nms, ~ !is.null(has[[.x]]))

  has$tmp <- if ((returns_null && !is_redu && !is_lmap) || is_extr_fn) TRUE else FALSE

  has

}

extract_is_args <- function(map_fn_chr, dir, fn_expr, q_env) {

  fn_ls <- list("back"    = !is.null(dir) && dir == "backward",
                "lmap"    = grepl("^lmap", map_fn_chr, perl = TRUE),
                "walk"    = grepl("^(walk|iwalk|pwalk)", map_fn_chr, perl = TRUE),
                "i"       = grepl("(^imap)|(^iwalk)|(^imodify)", map_fn_chr, perl = TRUE),
                "modify"  = grepl("modify", map_fn_chr, perl = TRUE),
                "accu"    = grepl("accumulate", map_fn_chr, perl = TRUE),
                "accu2"   = grepl("^accumulate2$", map_fn_chr, perl = TRUE),
                "redu"    = grepl("reduce", map_fn_chr, perl = TRUE),
                "extr_fn" = check_extr_fn(fn_expr, q_env),
                "pmap"    = grepl("(pmap)|(pwalk)", map_fn_chr, perl = TRUE)
               )

  fn_ls

}

check_and_try_call <- function(checks, null, force, q, map_fn, map_fn_chr, q_env, args_ls) {

  par_frame <- parent.frame()

  if (checks) {
    res <- try_purr_call(q, map_fn, map_fn_chr, par_frame)



    if (inherits(res, "purrr-error")) {
      assign("yields_error", TRUE, envir = par_frame)
    } else {
      assign("yields_error", FALSE, envir = par_frame)
    }

    if (null == "auto") {
      assign("return_null",
             any(purrr::map_lgl(res, is.null)),
             envir = par_frame)
    }

    if (force == "auto") {
      ln_res <- length(res)
      # for performance reasons, only test first and last element
      if(ln_res > 1) {
        res <- res[c(1, ln_res)]
      }
      assign("force_eval",
             any(purrr::map_lgl(res, ~ check_lazy(.x, q_env))),
             envir = par_frame)
    }
  } else {
    assign("yields_error", NULL, envir = par_frame)
    if (is.null(names(args_ls)) || any(nchar(names(args_ls)) == 0L)) {
      rlang::abort(
        c("Problem with `as_loop()` input `.expr`.",
          i = "When `as_loop` is called with `checks = FALSE` all arguments of the underlying {purrr} call must be named.",
          x = "Not all arguments in the underlying {purrr} call are named.",
          i = "Please name all arguments, e.g. `map(.x = ..., .f = ...).")
      )
    }
  }

}

reduce2accumulate <- function(expr) {

  # 1. Turn expression or quosure into list
  is_quosure <- rlang::is_quosure(expr)
  is_call    <- !is_quosure && is.call(expr)
  is_list    <- is.list(expr)

  stopifnot(is_quosure || is_call || is_list)

  if (is_quosure) initial_quo <- expr

  if (is_quosure) {
    expr <- rlang::get_expr(expr)
  }

  if (is.call(expr)) {
    expr <- as.list(expr)
  }

  # 2. Test if reduce or reduce2
  map_fn_chr  <- deparse_and_rm_nmspace(expr[[1]])
  is_redu     <- grepl("^reduce$",  map_fn_chr, perl = TRUE)
  is_redu2    <- grepl("^reduce2$", map_fn_chr, perl = TRUE)

  # 3. update expression and finalize as call or quosure if needed
  if (is_redu) {
    expr[[1]] <- rlang::expr(accumulate)
  } else if (is_redu2) {
    expr[[1]] <- rlang::expr(accumulate2)
  }

  if (is_list) {
    return(expr)
  }

  expr <- as.call(expr)

  if (is_call) {
    return(expr)
  }

  new_quo <- rlang::quo_set_expr(initial_quo, expr)
  new_quo
}

check_functionable <- function(x, fn_call, arg) {

  is_functionable <- rlang::is_function(try(rlang::as_function(x),
                                            silent = TRUE))

  if (fn_call == "screen") {
    msg <- c(i = paste0("`", arg, "` must be a list of:"),
       paste0("\033[32m", "\u23F5", "\033[39m", " function names"),
       paste0("\033[32m", "\u23F5", "\033[39m", " anonymous functions and/or"),
       paste0("\033[32m", "\u23F5", "\033[39m", " formulas that can be coerced with `rlang::as_function()`.")
       )
  } else {
    msg <- c(i = paste0("`", arg,"` must be either:"),
      paste0("\033[32m", "\u23F5", "\033[39m", " a function name"),
      paste0("\033[32m", "\u23F5", "\033[39m", " an anonymous function or"),
      paste0("\033[32m", "\u23F5", "\033[39m", " a formula that can be coerced with `rlang::as_function()`.")
    )
  }

  if(! is_functionable) {
    rlang::abort(
      c(paste0("Problem with `", fn_call, "()` input `", arg, "`."),
        msg,
        x = paste0("The input in `", arg, "` doesn't fullful this condition.")
      )
    )
  }
}

check_pos_integer <- function(x, cl, inp) {

  if (length(x) != 1L) {
    rlang::abort(c(paste0("Problem with `", cl, "()` input `", inp, "`."),
                   i = paste0("`", inp ,"` only accepts numeric vectors of length 1."),
                   x = paste0("`", inp, "` is of length ", length(x), ".")
    )
    )
  }

  if (!is.numeric(x)) {
    rlang::abort(c(paste0("Problem with `", cl, "()` input `", inp, "`."),
                    i = paste0("`", inp ,"` only accepts numeric values."),
                    x = paste0("`", inp, "` is of class ", toString(class(x)), ".")
    )
    )
  }

  if (sign(x) == -1L) {
    rlang::abort(c(paste0("Problem with `", cl, "()` input `", inp, "`."),
                   i = paste0("`", inp ,"` only accepts non-negative values."),
                   x = paste0("`", inp, "` is ", x, ".")
    )
    )
  }
}

check_bool <- function (x, cl, inp) {

  if (!missing(x) && rlang::is_bool(x)) {
    return(invisible(NULL))
  }

  rlang::abort(
    c(paste0("Problem with `", cl, "` input `", inp, "`."),
      i = paste0("`", inp, "` must be `TRUE` or `FALSE`."),
      x = paste0("`", inp, "` is of type `", class(x), "`.")
    )
  )
}

check_list <- function(x, cl, inp) {
  if (!is.list(x)) {
    rlang::abort(c(paste0("Problem with `", cl, "()` input `", inp, "`."),
                   i = paste0("`", inp ,"` has to be of type `list`."),
                   x = paste0("`", inp, "` is type ", typeof(x), ".")
    )
    )
  }
}

# sequence greater than one
# seq_gt_1 <- function(x) {
#   res <- seq_len(x)
#   if (length(res) > 1) res else ""
# }
#
# # number duplicate names
# number_names <- function(x) {
#   rle_x_ln <- rle(x)$length
#   res <- lapply(rle_x_ln, seq_gt_1)
#   numbers <- unlist(res)
#   paste0(x, numbers)
# }
#
# x <- c("result", "error", "result", "warning", "error")
# res <- make.names(c(x, "Best"), unique = TRUE)
# nms_series <- grep("\\.1", res, value = TRUE)
# nms_stem <- gsub("\\.1$", "", nms_series)
# res2 <- stringr::str_replace(res, paste0("^", nms_stem, "$"), paste0(nms_stem,"1"))
# res3 <- stringr::str_replace(res2, "\\.\\d+", function(x) )

