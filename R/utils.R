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

  supported_fns <- list(

    as_loop = list(

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
  )

  if (is.null(supported_fns[[fn]])) {
    rlang::inform("No supported functions. Did you specify the function name correctly?")
    return(invisible(NULL))
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

try_purr_call <- function(x, map_fn_chr) {
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
      c("Problem with `as_loop()` input `.expr`.",
        i = "Currently `as_loop` doesn't support functions from base R's apply family.",
        x = paste0("`", map_fn, "` is a function from base R's apply family."),
        i = "For an overview of all currently supported {purrr} functions see the documentation `?as_loop`.")
    )
    } else {
      FALSE
    }
  }

  if (!any(purrr::map_lgl(findFunction(map_fn), ~rlang::env_name(.x) == "package:purrr"))) {
    if (!silent) {
      rlang::abort(
        c("Problem with `as_loop()` input `.expr`.",
          i = "`as_loop` only works with `map` and similar functions from the purrr package.",
          x = paste0("`", map_fn, "` is not located in the namespace of `package:purrr`."),
          i = "For an overview of all currently supported {purrr} functions see the documentation `?as_loop`.")
      )
    } else {
      FALSE
    }
  }

  if (!map_fn %in% supported_fns) {
    if (!silent) {
      rlang::abort(
        c("Problem with `as_loop()` input `.expr`.",
          i = "Currently `as_loop` does only support certain {purrr} functions.",
          x = paste0("`", map_fn, "` is not supported yet."),
          i = paste0('For an overview of all currently supported {purrr} functions see the documentation `?as_loop` or ',
                     'run `get_supported_fns("as_loop")`'))
      )
    } else {
      FALSE
    }
  }
  map_fn %in% supported_fns
}

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
