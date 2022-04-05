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

      map = c("map2", "map2_chr", "map2_dbl", "map2_df", "map2_dfc", "map2_dfr", "map2_int",
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


try_purr_call <- function(x, map_fn_chr) {
  tryCatch({
    sink(nullfile()) # "/dev/null"
    on.exit(sink(), add = TRUE)
    tmp <- rlang::eval_tidy(x)
    # sink()
    tmp
  }, error = function(e) {
    rlang::abort(c("Problem with `as_loop()` input `.expr`.",
                   i = paste0("The underlying call to `purrr::", map_fn_chr,"` threw the following error:"),
                   x = e$message,
                   i = "Please provide a working call to `as_loop`.")
    )
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

  print(start)
  print(end)

  loc <- if (identical(start, end) && start[2] == 1L) {
    print("1")
    print(start[1])
    out <- calc_last_line(context, start)
    list(out, NULL)
  } else if (end[2] == 1L) {
    print("2")
    return(list(end[1], NULL))
  } else {
    # when expression is marked
    print("3")
    list(end[1] + 1L, end[1] + 1L + x_ln)
  }
  print(loc)
  loc
}

calc_last_line <- function(context, loc) {

  con <- context$contents
  con_df <- dplyr::tibble(line = seq_along(con), code = con)

  following_lines <- con_df %>%
    dplyr::filter(dplyr::row_number() > loc[1]) %>%
    dplyr::pull(code)

  print(following_lines)

  only_empty_lines <- !as.logical(sum(nchar(following_lines)))

  print(only_empty_lines)

  last_filled_line <- if(only_empty_lines) {
    loc[1] - 1
  } else {
    con_df %>%
      dplyr::filter(nzchar(gsub("\\s+", "", code))) %>%
      dplyr::filter(dplyr::lead(line) == loc[1]) %>%
      dplyr::pull(line)
  }

  print(last_filled_line + 1)
  last_filled_line + 1L
}


is_supported <- function(map_fn, loopurrr_fn) {

  supported_fns <- unlist(get_supported_fns(loopurrr_fn))

  not_supported_fns <- c("apply", "lapply", "vapply", "sapply", "rapply", "Map", "mapply", "tapply")

  if (map_fn %in% not_supported_fns) {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        i = "Currently `as_loop` doesn't support functions from base R's apply family.",
        x = paste0("`", map_fn, "` is a function from base R's apply family."),
        i = "For an overview of all currently supported {purrr} functions see the documentation `?as_loop`.")
    )
  }

  if (!any(purrr::map_lgl(findFunction(map_fn), ~rlang::env_name(.x) == "package:purrr"))) {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        i = "`as_loop` only works with `map` and similar functions from the purrr package.",
        x = paste0("`", map_fn, "` is not located in the namespace of `package:purrr`."),
        i = "For an overview of all currently supported {purrr} functions see the documentation `?as_loop`.")
    )
  }

  if (!map_fn %in% supported_fns) {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        i = "Currently `as_loop` does only support certain {purrr} functions.",
        x = paste0("`", map_fn, "` is not supported yet."),
        i = "For an overview of all currently supported {purrr} functions see the documentation `?as_loop`.")
    )
  }

}

check_syntactical_nm <- function(x) {
  grepl("(^(\\p{L}|(\\.(\\p{L}|\\.|\\_)))(\\d|\\p{L}|\\.|\\_)*$)|(^\\.$)", x, perl = TRUE)
}

check_extr_fn <- function(fn_expr, e) {
  fn <- eval(fn_expr, envir = e)
  check <- is.numeric(fn) || is.character(fn)
  check
}

