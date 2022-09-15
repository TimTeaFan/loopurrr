# TODO: document and export this function
# TODO: add better error messages
# TODO: add tests

# Tests:
# a <- list(1, 1:2, 1:3)
# map(a, sum) %>% ping(1)
# map(a, sum) %>% ping(c(1,3))
# map(a, sum) %>% ping(simplify = FALSE)
# ping rewritten based on wrap
ping <- function(.expr, i = 1L, simplify = TRUE) {

  q <- rlang::enquo(.expr)

  # check and unpipe
  q <- unpipe_expr(q,
                   sc = sys.calls(),
                   is_dot = match.call()$`.expr` == ".",
                   calling_fn = "ping")

  check_names <- check_i(i, "ping")

  out <- wrap(.expr = q,
              .x = index_fn(i),
              .y = index_fn(i),
              .l = index_each_fn(i),
              silent = TRUE,
              `.__impl__.` = TRUE,
              `.__check__.`= check_names
  )

  if (simplify && is.list(out) && length(out) == 1L) {
    out <- unlist(out, recursive = FALSE)
  }

  out

}

get_first_el <- function(x) {
  if (depth(x) > 1) {
    get_first_el(x[[1]])
  } else {
    return(x[[1]])
  }
}

depth <- function(this, thisdepth=0){
  if(!is.list(this)){
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,depth,thisdepth=thisdepth+1))))
  }
}

check_last_call <- function(calls, fn) {

  call_ls <- call_to_list(calls)
  # cl_depth <- depth(call_ls)
  first_sym <- get_first_el(call_ls)
  out <- FALSE

  if (as.character(first_sym) == fn) {
    out <- TRUE
  } else if (as.character(first_sym) == "%>%") {
    first_call <- call_ls[[1]]
    first_cl_ln <- length(first_call)
    end_of_ls_call <- first_call[[first_cl_ln]][[1]]
    if (as.character(end_of_ls_call) == fn) {
      out <- TRUE
    }
  }
  out
}

# Transform a list of call into a nested list of symbols
call_to_list <- function(call) {

  call_ls <- as.list(call)

  if (all(purrr::map_lgl(call_ls, is.symbol))) {
    return(call_ls)
  } else if (length(call_ls) == 1L && is.symbol(call_ls[[1]])) {
    return(call_ls)
  } else {
    for (i in seq_along(call_ls)) {
    call_ls[[i]] <- Recall(call_ls[[i]])
    }
    return(call_ls)
  }
}

probe <- function(.expr) {

  q <- rlang::enquo(.expr)

  # check and unpipe
  q <- unpipe_expr(q,
                   sc = sys.calls(),
                   is_dot = match.call()$`.expr` == ".",
                   calling_fn = "probe")

  i <- first_error_imp(q)

  if (is.null(i)) {
    return(rlang::inform(paste0("No error detected.")))
  } else {

    q_expr <- rlang::quo_get_expr(q)
    q_env <- rlang::quo_get_env(q)

    map_fn_chr <- deparse(q_expr[[1]])

    if (grepl("^\\w+::", map_fn_chr, perl = TRUE)) {
      map_fn_chr <- gsub("^\\w+::", "", map_fn_chr)
    }

    map_fn <- get(map_fn_chr, envir = rlang::as_environment("purrr"))
    q_ex_std <- match.call(definition = map_fn, call = q_expr)

    expr_ls <- as.list(q_ex_std)

    obj_x <- eval(expr_ls[[".x"]], envir = q_env)

    # FIXME: Only print `inform` If last call on the stack:
    last_call <- check_last_call(sys.calls(), "probe")

    if (last_call) {
      rlang::inform(paste0("The first error is thrown by element no. ", i, ".\n",
                         "This object is returned invisible."))
    }
    invisible(`[[`(obj_x, i))
  }
}


# TODO: Make function work with the first and last element `.f` (and or `.else`) was applied to:
first_and_last <- function(x) {
  ln_x <- length(x)
  if (ln_x > 1) {
    c(1, ln_x)
  } else if (ln_x == 1L) {
    1L
  } else {
    stop("Object must have 1 or more elements.")
  }
}

inspect <- function(x) {

  cat("structure:\n")
  cat(str(x))

  tibble::tibble(length = length(x),
                 class  = class(x),
                 type   = typeof(x))
}

first_error <- function() {
  # add error that first_error cannot be called outside of ping
  first_error
}

first_error_imp <- function(.expr) {
  res <- wrap(.expr,
              .f = safely,
              `.__impl__.` = TRUE)
  error <- purrr::transpose(res)$error
  idx <- !purrr::map_lgl(error, is.null)

  if (any(idx)) {
    which(idx)[1]
  } else (
    NULL
  )
}

index_fn <- function(j) {
  function(x, i = j) `[`(x, i)
}

index_each_fn <- function(j) {
  function(x, i = j) {
    purrr::map(x, function(y) `[`(y, i))
  }
}

wrap <- function(.expr, ..., silent = FALSE) {

  dots <- rlang::list2(...)

  # checks if wrap is used to implement another function
  implement <- !is.null(dots[[".__impl__."]])
  # when `ping` calls `wrap` names have to be checked once the calls is standardized (see below)
  i_names <- dots[[".__check__."]]
  check_names <- !is.null(i_names)
  # the hidden args should not be part of the dots
  dots <- dots[-which(names(dots) %in% c(".__impl__.", ".__check__."))]

  if (!rlang::is_named(dots)) {
    stop("All arguments in the ellipsis `...` must be named.")
  }

  # `.__impl__.` is a hidden arg used when other functions call `wrap`
  # in this case .expr is already a preprocessed quosure
  # The arg it isn't needed when the user calls `wrap()` directly
  if (implement) {
    q <- .expr
  } else {
    q <- rlang::enquo(.expr)

    # check and unpipe
    q <- unpipe_expr(q,
                     sc = sys.calls(),
                     is_dot = match.call()$`.expr` == ".",
                     calling_fn = "wrap")
  }

  q_expr <- rlang::quo_get_expr(q)
  q_env <- rlang::quo_get_env(q)

  map_fn_chr <- deparse(q_expr[[1]])

  if (grepl("^\\w+::", map_fn_chr, perl = TRUE)) {
    map_fn_chr <- gsub("^\\w+::", "", map_fn_chr)
  }

  map_fn <- get(map_fn_chr, envir = rlang::as_environment("purrr"))
  q_ex_std <- match.call(definition = map_fn, call = q_expr)

  expr_ls <- as.list(q_ex_std)

  if (check_names && !all(i_names %in% names(eval(expr_ls[[".x"]], envir = q_env)))) {
    stop("Problem with ping(). Not all names in object `.x`")
  }

  for (arg in names(dots)) {
    if (!arg %in% names(expr_ls)) {
     if(silent) next else stop(paste0("Argument `", arg, "` doesn't exist in `.expr`."))
    }
    expr_ls[[arg]] <- rlang::call2(dots[[arg]], expr_ls[[arg]])
  }

  eval(as.call(expr_ls))
}


# implement a function like this
# but including input class and output class
# https://github.com/tidyverse/purrr/issues/843
# screen

# wrap_.f <- function(.expr, fn) {
#
#   q <- rlang::enquo(.expr)
#
#   # check and unpipe
#   new_expr <- check_and_unpipe(sys.calls(),
#                                is_dot = match.call()$`.expr` == ".",
#                                calling_fn = "wrap_.f")
#
#   if (!is.null(new_expr)) {
#     q <- rlang::quo_set_expr(q, new_expr)
#   }
#
#   q_expr <- rlang::quo_get_expr(q)
#   q_env <- rlang::quo_get_env(q)
#
#   map_fn_chr <- deparse(q_expr[[1]])
#
#   if (grepl("^\\w+::", map_fn_chr, perl = TRUE)) {
#     map_fn_chr <- gsub("^\\w+::", "", map_fn_chr)
#   }
#
#   map_fn <- get(map_fn_chr, envir = rlang::as_environment("purrr"))
#   q_ex_std <- match.call(definition = map_fn, call = q_expr)
#
#   expr_ls <- as.list(q_ex_std)
#
#   expr_ls[[".f"]] <- rlang::call2(fn, expr_ls[[".f"]])
#
#   eval(as.call(expr_ls))
# }
#
# old_ping <- function(.expr, i = 1L, simplify = TRUE) {
#
#   q <- rlang::enquo(.expr)
#
#   # check and unpipe
#   q <- unpipe_expr(q,
#                    sc = sys.calls(),
#                    is_dot = match.call()$`.expr` == ".",
#                    calling_fn = "ping")
#
#   q_expr <- rlang::quo_get_expr(q)
#   q_env <- rlang::quo_get_env(q)
#
#   map_fn_chr <- deparse(q_expr[[1]])
#
#   if (grepl("^\\w+::", map_fn_chr, perl = TRUE)) {
#     map_fn_chr <- gsub("^\\w+::", "", map_fn_chr)
#   }
#
#   map_fn <- get(map_fn_chr, envir = rlang::as_environment("purrr"))
#   q_ex_std <- match.call(definition = map_fn, call = q_expr)
#
#   expr_ls <- as.list(q_ex_std)
#
#   if (!is.null(expr_ls[[".x"]])) {
#     expr_ls[[".x"]] <- rlang::call2(`[`, expr_ls[[".x"]], i)
#   }
#
#   if (!is.null(expr_ls[[".y"]])) {
#     expr_ls[[".y"]] <- rlang::call2(`[`, expr_ls[[".y"]], i)
#   }
#
#   if (!is.null(expr_ls[[".l"]])) {
#
#     index_each_el <- rlang::as_function(~ purrr::map(.x, function(x) `[`(x, i)))
#
#     expr_ls[[".l"]] <- rlang::call2(index_each_el, expr_ls[[".l"]])
#   }
#
#   out <- eval(as.call(expr_ls))
#
#   if (simplify && is.list(out) && length(out) == 1L) {
#     out <- unlist(out, recursive = FALSE)
#   }
#
#   out
#
# }
