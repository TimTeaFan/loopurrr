#' Test purrr's iterator functions on a subset of input elements
#'
#' @description
#' `peek()` takes a function call to one of `{purrr}`'s iterator functions, such as [purrr::map()],
#' and tests if the function call can be run on a selected subset `i` of input elements
#' (`.x`, `.y` or `.l`). Without further arguments `peek()` will always test the first input
#' element(s) (`i = 1`).
#'
#' `peek()` will modify the iterator function call so that it is only applied to the selected elements
#' in `i`. This means, errors that might occur in non-selected elements will not be thrown, since
#' the input elements are subsetted before the evaluation of the function call.
#'
#' `peek()` will also adjust all index elements of the original call to reproduce the same output,
#' as if the output had been subsetted by `i` after the function call has been evaluated.
#' For example the `.at` argument in [purrr::map_at()] or the `.y` object in the `.f` argument
#' of [purrr::imap()]. See the example section for more details.
#'
#'
#' @param expr A function call to a `{purrr}` iterator function. See the "Supported functions"
#' section below for an overview of which `{purrr}` iterator functions are currently supported.
#'
#' @param i index specifying elements to extract or replace. Only numeric or character vectors are
#' excepted. When numeric, the values have to be all positive or all negative. Neither numeric vectors
#' with positive and negative numbers nor lists mixing numeric and character element can be used
#' for subsetting.
#'
#' @param simplify When `TRUE` (the default) and when `i` is of length one, `peek()` will `unname`
#' and `unlist` the output object. When `FALSE` the output object is return as is.
#'
#' @returns
#' A subset of the output of the original function call to a `{purrr}` iterator function. When
#' the output is of length one and `simplify = TRUE` the output object will be unnamed and unlisted.
#'
#' When the input iterator function is [purrr::reduce()] or [purrr::reduce2()] and `i` selects more
#' than one element, all selected elements are returned (unlike the originial `reduce` call which
#' always return the last element only)
#'
#'
#' @section Supported functions:
#' ```{r, child = "man/rmd/setup.Rmd", eval = FALSE}
#' ```
#'
#' ```{r, echo = FALSE}
#' ```
#'
#' The following iterator functions from the `{purrr}` package are currently supported:
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = TRUE}
#' get_supported_fns("peek")
#' ```
#'
#' @section Examples:
#' If called without arguments, `peek()` will subset the first element. It will further
#' unname and unnest the output. We can prevent this behavior by using `simplify = FALSE`.
#' If `i` is of length greater than one, the output will never be unnamed and unlisted.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' a <- list(a = 1:3, b = 4:6, c = 7:9)
#'
#' map(a, sum) %>% peek()
#'
#' map(a, sum) %>% peek(simplify = FALSE)
#'
#' map(a, sum) %>% peek(c(1,3))
#' ```
#'
#' `peek()` subsets the input elements before evaluating the call. So errors in non-selected
#' elements won't be thrown. Compare:
#'
#' ```{r, comment = "#>", collapse = TRUE, error = TRUE}
#' b <- list(a = 1:3, b = letters[4:6], c = 7:9)
#'
#' map(b, sum)
#'
#' map(b, sum) %>% peek(c(1, 3))
#' ```
#'
#' Used on a simple call to [purrr::map()] `peek()` does nothing more than subsetting the `.x` argument
#' before evaluating the call. When used on [purrr::map_at()] or [purrr::imap()] `peek()` will adjust
#' all index elements to produce a result as if the output of the call had been subsetted.
#'
#' ```{r, comment = "#>", collapse = TRUE, error = TRUE}
#' a <- list(a = 1:3, b = 4:6, c = 7:9)
#'
#' map(a, sum) %>% peek(2)
#'
#' # the above is the same as
#' map(a[2], sum)
#'
#' # this doesn't hold true for `map_at()` or `imap()`:
#' map_at(a, .at = 2, sum)
#'
#' # this works correctly
#' map_at(a, .at = 2, sum) %>% peek(2)
#'
#' # this doesn't throw an error, ...
#' # but applies `sum` to a non existing element, returning `a[2]` as is:
#' map_at(a[2], .at = 2, sum)
#'
#'
#' # the same holds for `imap()`:
#' b <- unname(a)
#' imap(b, ~ paste(.y, ":", sum(.x)))
#'
#' # returns the second element of above's call
#' imap(b, ~ paste(.y, ":", sum(.x))) %>% peek(2)
#'
#' # while `sum(.x)` corresponds to the second element ...
#' # the index in why `.y` has changed:
#' imap(b[2], ~ paste(.y, ":", sum(.x)))
#' ```
#'
#' Using `peek()` on [purrr::reduce()] and [purrr::reduce2()] is a special case, for two reasons:
#'
#' 1. We can select more than one element in `i` and all selected elements will be returned
#' (usually `reduce` only returns the last element of the iteration).
#'
#' 2. Since each element depends on the preceding output element, `peek()` will evaluate all
#' needed elements to produce the desired output, even if they haven't been selected.
#'
#' The second point also applies to [purrr::accumulate()] and [purrr::accumulate2()].
#'
#' The following example should illustrate both points:
#'
#' ```{r, comment = "#>", collapse = TRUE, error = TRUE}
#' a <- as.list(1:10)
#'
#' reduce(a, ~ sum(.x, .y))
#'
#' # This will return and evaluate the first element only
#' reduce(a, ~ sum(.x, .y)) %>% peek()
#'
#' # This will evaluate element 1 to 3 but only return 2 and 3
#' reduce(a, ~ sum(.x, .y)) %>% peek(c(2,3))
#'
#' b <- c(as.list(1:3), "d", as.list(5:7))
#'
#' # only evaluated the first three elements (no problem)
#' reduce(b, ~ sum(.x, .y)) %>% peek(c(1, 3))
#'
#' # evaluates elements 2 to 5, an error eccours in element 4
#' reduce(b, ~ sum(.x, .y)) %>% peek(c(2, 5))
#' ```
#' @export
peek <- function(expr, i = 1L, simplify = TRUE) {

  q <- rlang::enquo(expr)

  # check and unpipe
  q <- unpipe_expr(q,
                   sc = sys.calls(),
                   is_dot = match.call()$expr == ".",
                   calling_fn = "peek")

  q_expr        <- rlang::quo_get_expr(q)
  q_env         <- rlang::quo_get_env(q)

  map_fn_chr    <- deparse_and_rm_nmspace(q_expr[[1]])
  map_fn        <- mabye_check_map_fn(map_fn_chr, "peek", checks = TRUE)
  expr_ls       <- reformat_expr_ls(q_expr = q_expr, fn = map_fn)

  is_imap       <- grepl("(^imap)|(^iwalk)|(^imodify)", map_fn_chr, perl = TRUE)
  is_walk       <- grepl("^(walk|iwalk|pwalk)", map_fn_chr, perl = TRUE)
  is_redu       <- grepl("reduce", map_fn_chr, perl = TRUE)
  is_accu       <- grepl("^accumulate", map_fn_chr, perl = TRUE)
  is_accu_redu2 <- grepl("^(accumulate|reduce)2", map_fn_chr, perl = TRUE)
  is_back       <- !is.null(expr_ls[[".dir"]]) && expr_ls[[".dir"]] == "backward"



  has_init      <- !is.null(expr_ls[[".init"]])

  # get object .x or .l[[1]]
  obj <- get_obj(expr_ls, q_env)
  obj_len <- length(obj)

  # preserve original i
  old_i <- i

  # perform checks and convert, adjust i if necessary
  i <- check_and_convert_i(i = i,
                           main_obj = obj,
                           calling_fn = "peek",
                           is_redu = is_redu,
                           is_accu = is_accu,
                           has_init = has_init,
                           is_back = is_back)

  # adjust old_i if reduce or accumulate (here is must be numeric)
  if (is_redu || is_accu) {
    old_i <- old_i + !is_back
  }

  drop_names <- FALSE
  if (is_redu && length(old_i) > 1L) {
    expr_ls[[1]] <- if (is_accu_redu2) rlang::expr(accumulate2) else rlang::expr(accumulate)
    is_accu <- TRUE
    q <- rlang::quo_set_expr(q, as.call(expr_ls))
    # when changing reduce to accumulate we have to make sure it drops names
    drop_names <- TRUE
  }

  # create transform_at
  at_fun <- NULL

  if (!is.null(expr_ls[[".at"]])) {
    at_fun <- transfrom_at(i, obj)
  }

  fn <- maybe_transfrom_fn(i, is_imap, names(obj))

  if (is_walk) {
    return(
      wrap("..expr" = !! q,
         .x = index_fn(i),      # function(x, i) x[i],
         .y = index_fn(i),      # function(x, i) x[i],
         .l = index_each_fn(i), # function(x, i) purrr::map(x, function(y) `[`(y, i)),
         .at = at_fun,
         .f = fn,
         "..silent" = TRUE)
    )
  }

  out <- wrap("..expr" = !! q,
              .x = index_fn(i),
              .y = index_fn(i, one_less = is_accu_redu2 & !has_init),
              .l = index_each_fn(i),
              .at = at_fun,
              .f = fn,
              "..silent" = TRUE
  )

  # return only the original i's
  if (is_accu) {
    if (is_back) {
      out <- rev(out)[-1][old_i]
    } else {
      out <- out[old_i]
    }
  }

  if (drop_names) {
    out <- unname(out)
  }

  if (simplify && is.list(out) && length(out) == 1L) {

    out <- unlist(unname(out), recursive = FALSE)

    # if the unlisted output is not a list anymore: drop names
    if (!is.list(out) && !is.null(names(out))) {
      out <- unname(out)
      rlang::inform(c(i = "`peek()` has dropped output names. You can override using `simplify = FALSE`."))
    }
  }

  out

}

# ------------------------------ #
# peek()'s helper functions ----
# ------------------------------ #

# index a vector
index_fn <- function(j, one_less = FALSE) {
  j <- j - one_less
  function(x, i = j) {
    x_att <- attributes(x)
    copy_att <- x_att[!names(x_att) %in% c("names", "dim", "dimnames")]
    out <- `[`(x, i)
    attributes(out) <- append(attributes(out), copy_att)
    out
    }

}

# index a list of lists
index_each_fn <- function(j) {

  function(x, i = j) {
    purrr::map(x, function(y) `[`(y, i))
  }

}

# transform at argument to match peeks subsets
transfrom_at <- function(i, obj) {

  out <- function(org_at, x_seq = seq_along(obj), j = i) {

    if (is.numeric(org_at)) {
      which(is.element(x_seq, org_at)[j])
    } else {
      org_at
    }
  }
  out
}

# transfrom fn in .f to make peek work with imap etc.
maybe_transfrom_fn <- function(i, is_imap, obj_nms) {

  out <- function(org_fn) org_fn

  # if imap and numeric (non-names) index
  if (is_imap && is.null(obj_nms)) {

    # this part is tough:
    # we need to wrap the original function into a function taking two arguments plus dots
    # we want to transform the index, so that it reflects peek's i value and not the original .y
    out <- function(org_fn) {
      # might be a formula so first turn it into a real function
      org_fn <- rlang::as_function(org_fn)
      function(x, y, ...) org_fn(x, i[y], ...)
    }

  }
  out
}

# TODO: Make function work with the first and last element `.f` (and or `.else`) was applied to:
# first_and_last <- function(x) {
#   ln_x <- length(x)
#   if (ln_x > 1) {
#     c(1, ln_x)
#   } else if (ln_x == 1L) {
#     1L
#   } else {
#     stop("Object must have 1 or more elements.")
#   }
# }

# get the object of map or pmap call's
get_obj <- function(expr_ls, q_env) {

  if (!is.null(expr_ls[[".x"]])) {
    obj_expr <- expr_ls[[".x"]]
  } else {
    obj_expr <- rlang::call2("[[", expr_ls[[".l"]], 1L)
  }

  obj <- eval(obj_expr, envir = q_env)

}

# checks i argument in peek and converts i to positive integers
check_and_convert_i <- function(i, main_obj, obj = main_obj, calling_fn, is_redu, is_accu, has_init, is_back) {

  # check class
  if (!is.numeric(i) && ! is.character(i)) {
    stop(paste0("`", calling_fn ,"()` only accepts numeric or character vectors for subsetting."))
  }

  # check class for reduce and accumulate (doesn't accept names)
  if ((is_redu || is_accu) && !is.numeric(i)) {
    stop(paste0("`", calling_fn ,"()` only accepts numeric vectors for indexing `reduce()` and `accumulate()` calls."))
  }

  # check against mixed subsetting (positve and negative)
  if (is.numeric(i) && any(sign(i) == 1L) && any(sign(i) == -1L)) {
    stop(paste0("`", calling_fn ,"()`'s `i` argument must not contain positive and negative subscripts."))
  }

  # checks against obj: idx in obj?
  if (is.numeric(i) && any(!abs(i) %in% seq_len(has_init + length(obj) - (is_redu || is_accu) ))) {
    stop(paste0("Problem with `", calling_fn ,"()`. One or more locations don't exist."))
  }

  # checks against obj: names in obj
  if (is.character(i) && any(!i %in% names(main_obj))) {
    stop("Problem with `", calling_fn ,"()`. One or more named elements don't exist.")
  }

  # convert subsetting by name to numeric subscripts
  if (is.character(i)) {
    i <- which(is.element(names(main_obj), i))
  }

  # convert negative subscripts to positive subscripts
  if (is.numeric(i) && all(sign(i) == -1L)) {
    i <- which(!is.element(seq_len(has_init + length(obj)), abs(i)))
  }

  # if reduce or accumulate we need all i's from 1 to the max i
  if (is_redu || is_accu) {
    max_i <- max(i) - ((calling_fn == "peek") * has_init)
    i <- seq_len(max_i)
    if (calling_fn == "peek") {
      i <- append(i, max_i + 1)
    }
    if (is_back) {
      i <- i + (length(main_obj) - max(i))
    }
  }
  i
}
