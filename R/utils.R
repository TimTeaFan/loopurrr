
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
                     "accumulate2", "reduce")

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
