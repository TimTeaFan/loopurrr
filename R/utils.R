
# https://stackoverflow.com/a/14295659/9349302
# not needed anymore
# find_calls <- function(x) {
#   # Base case
#   if (!is.recursive(x)) return()
#
#   recurse <- function(x) {
#     sort(unique(as.character(unlist(lapply(x, find_calls)))))
#   }
#
#   if (is.call(x)) {
#     f_name <- as.character(x[[1]])
#     c(f_name, recurse(x[-1]))
#   } else {
#     recurse(x)
#   }
# }


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
  con <<- context$contents
  con_df <- dplyr::tibble(line = seq_along(con), code = con)
  last_filled_line <- con_df %>%
    dplyr::filter(nzchar(gsub("\\s+", "", code))) %>%
    dplyr::filter(dplyr::lead(line) == loc[1]) %>%
    dplyr::pull(line)
  last_filled_line + 1L
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
                     "pmap_int", "pmap_lgl", "pmap_raw", "pwalk", "walk", "walk2", "accumulate",
                     "accumulate2", "reduce", "reduce2")

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


# return_wo_print <- function(fn_call) {
#   sink("/dev/null")
#   res <- fn_call
#   sink()
#   res
# }
