wrap <- function(..expr, ..., ..silent = FALSE) {

  dots <- rlang::list2(...)

  # strip NULL args
  dots <- compact(dots)

  if (!rlang::is_named(dots)) {
    stop("All arguments in the ellipsis `...` must be named.")
  }

  q <- rlang::enquo(..expr)

  # check and unpipe
  q <- unpipe_expr(q,
                   sc = sys.calls(),
                   is_dot = match.call()$`..expr` == ".",
                   calling_fn = "wrap")

  q_expr <- rlang::quo_get_expr(q)
  q_env <- rlang::quo_get_env(q)

  map_fn_chr <- deparse_and_rm_nmspace(q_expr[[1]])

  map_fn <- mabye_check_map_fn(map_fn_chr, "screen", checks = TRUE)

  q_ex_std <- match.call(definition = map_fn, call = q_expr)

  expr_ls <- as.list(q_ex_std)

  for (arg in names(dots)) {
    if (!arg %in% names(expr_ls)) {
      if (..silent) next else stop(paste0("Argument `", arg, "` doesn't exist in `..expr`."))
    }
    expr_ls[[arg]] <- rlang::call2(dots[[arg]], expr_ls[[arg]])
  }

  eval(as.call(expr_ls), envir = q_env)
}

adjust_i <- function(i, is_redu_accu, has_init) {
  i - is_redu_accu + has_init
}

infer_possible_i <- function(main_obj, is_redu_accu, has_init) {

  obj_len <- length(main_obj)

  adjust_i(obj_len, is_redu_accu, has_init)

}



# unused funcitons

# function_to_call <- function(fn, sym) {
#
#   fn_bdy <- body(fn)
#   fn_env <- environment(fn)
#   fn_frml_nms <- rlang::fn_fmls_names(fn)
#
#   key_val_pairs <- create_key_val_pairs(sym, fn_frml_nms, fn_env)
#
#   replace_vars(fn_bdy, key_val_pairs)
#
# }

# create_key_val_pairs <- function(sym, fn_frml_nms, fn_env) {
#
#   # TODO: check if formals contain dots and if so throw error
#
#   out <- imap(fn_frml_nms, ~ {
#     if (.y == 1) {
#       sym
#     } else {
#       get(.x, envir = fn_env)
#     }
#   })
#
#   set_names(out, fn_frml_nms)
# }

# get_first_el <- function(x) {
#   if (depth(x) > 1) {
#     get_first_el(x[[1]])
#   } else {
#     return(x[[1]])
#   }
# }

# from https://stackoverflow.com/a/13433689/9349302
# depth <- function(this, thisdepth = 0){
#   if (!is.list(this)){
#     return(thisdepth)
#   } else {
#     max(unlist(lapply(this, depth, thisdepth = thisdepth + 1)))
#   }
# }

# check_last_call <- function(calls, fn) {
#
#   call_ls <- call_to_list(calls)
#   first_sym <- get_first_el(call_ls)
#   out <- FALSE
#
#   if (as.character(first_sym) == fn) {
#     out <- TRUE
#   } else if (as.character(first_sym) == "%>%") {
#     first_call <- call_ls[[1]]
#     first_cl_ln <- length(first_call)
#     end_of_ls_call <- first_call[[first_cl_ln]][[1]]
#     if (as.character(end_of_ls_call) == fn) {
#       out <- TRUE
#     }
#   }
#   out
# }


# Transform a list of calls into a nested list of symbols
# call_to_list <- function(call) {
#
#   call_ls <- as.list(call)
#
#   if (all(purrr::map_lgl(call_ls, is.symbol))) {
#     return(call_ls)
#   } else if (length(call_ls) == 1L && is.symbol(call_ls[[1]])) {
#     return(call_ls)
#   } else {
#     for (i in seq_along(call_ls)) {
#     call_ls[[i]] <- Recall(call_ls[[i]])
#     }
#     return(call_ls)
#   }
# }


# inspect <- function(x) {
#
#   cat("structure:\n")
#   cat(str(x))
#
#   tibble::tibble(length = length(x),
#                  class  = class(x),
#                  type   = typeof(x))
# }
