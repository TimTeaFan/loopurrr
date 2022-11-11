# TODO: document and export this function
# TODO: add better error messages
# TODO: add tests


ping <- function(expr, i = 1L, simplify = TRUE, ...) {

  dots <- rlang::list2(...)

  q <- rlang::enquo(expr)

  # check and unpipe
  q <- unpipe_expr(q,
                   sc = sys.calls(),
                   is_dot = match.call()$expr == ".",
                   calling_fn = "ping")

  q_expr <- rlang::quo_get_expr(q)
  q_env  <- rlang::quo_get_env(q)

  map_fn_chr <- deparse_and_rm_nmspace(q_expr[[1]])

  is_imap <- grepl("(^imap)|(^iwalk)|(^imodify)", map_fn_chr, perl = TRUE)
  is_walk <- grepl("^(walk|iwalk|pwalk)", map_fn_chr, perl = TRUE)
  is_redu <- grepl("reduce", map_fn_chr, perl = TRUE)
  is_accu <- grepl("^accumulate", map_fn_chr, perl = TRUE)
  is_accu_redu2 <- grepl("^(accumulate|reduce)2", map_fn_chr, perl = TRUE)

  map_fn  <- mabye_check_map_fn(map_fn_chr, "ping", checks = FALSE)

  expr_ls <- reformat_expr_ls(q_expr = q_expr, fn = map_fn)

  has_init <- !is.null(expr_ls[[".init"]])

  # get object .x or .l[[1]]
  obj <- get_obj(expr_ls, q_env)

  # preserve original i
  old_i <- i
  # perform checks and convert, adjust i if necessary
  i <- check_and_convert_i(i, obj, calling_fn = "ping",
                           is_redu = is_redu, is_accu = is_accu, has_init = has_init)

  if (!identical(old_i, i)) {
    rlang::inform(i = paste0("ping()` applied `.f` to all elements up to index ",
                             max(old_i),
                             ".")
                  )
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
              .y = index_fn(i, is_accu_redu2),
              .l = index_each_fn(i),
              .at = at_fun,
              .f = fn,
              "..silent" = TRUE
  )

  # return only the original i's
  if (is_accu) {
    out <- out[old_i]
  }

  if (drop_names) {
    out <- unname(out)
  }

  if (simplify && is.list(out) && length(out) == 1L) {
    if (!is.null(names(out))) rlang::inform(c(i = "`ping()` has dropped output names. You can override using `simplify = FALSE`."))
    out <- unname(unlist(out, recursive = FALSE))
  }

  out

}

# ------------------------------ #
# ping()'s helper functions ----
# ------------------------------ #

# index a vector
index_fn <- function(j, one_less = FALSE) {
  j <- j - one_less
  function(x, i = j) `[`(x, i)

}

# index a list of lists
index_each_fn <- function(j) {

  function(x, i = j) {
    purrr::map(x, function(y) `[`(y, i))
  }

}

# transform at argument to match pings subsets
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

# transfrom fn in .f to make ping work with imap etc.
maybe_transfrom_fn <- function(i, is_imap, obj_nms) {

  out <- function(org_fn) org_fn

  # if imap and numeric (non-names) index
  if (is_imap && is.null(obj_nms)) {

    # this part is tough:
    # we need to wrap the original function into a function taking two arguments plus dots
    # we want to transform the index, so that it reflects ping's i value and not the original .y
    out <- function(org_fn) {
      # might be a formula so first turn it into a real function
      org_fn <- rlang::as_function(org_fn)
      function(x, y, ...) org_fn(x, i[y], ...)
    }

  }
  out
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

# get the object of map or pmap call's
get_obj <- function(expr_ls, q_env) {

  if (!is.null(expr_ls[[".x"]])) {
    obj_expr <- expr_ls[[".x"]]
  } else {
    obj_expr <- rlang::call2("[[", expr_ls[[".l"]], 1L)
  }

  obj <- eval(obj_expr, envir = q_env)

}

# checks i argument in ping and converts i to positive integers
check_and_convert_i <- function(i, obj, calling_fn, is_redu, is_accu, has_init) {

  # check class
  if (!is.numeric(i) && ! is.character(i)) {
    stop(paste0("`", calling_fn ,"()` only accepts numeric or character vectors for subsetting."))
  }

  # check against mixed subsetting (positve and negative)
  if (is.numeric(i) && any(sign(i) == 1L) && any(sign(i) == -1L)) {
    stop(paste0("`", calling_fn ,"()`'s `i` argument must not contain positive and negative subscripts."))
  }

  # checks against obj: names in obj, idx in obj?
  if (is.numeric(i) && any(!abs(i) %in% seq_len(has_init + length(obj)))) {
    stop(paste0("Problem with `", calling_fn ,"()`. One or more locations don't exist."))
  }

  if (is.character(i) && any(!i %in% names(obj))) {
    stop("Problem with `", calling_fn ,"()`. One or more named elements don't exist.")
  }

  # convert subsetting by name to numeric subscripts
  if (is.character(i)) {
    i <- which(is.element(names(obj), i))
  }

  # convert negative subscripts to positive subscripts
  if (is.numeric(i) && all(sign(i) == -1L)) {
    i <- which(!is.element(seq_len(has_init + length(obj)), abs(i)))
  }

  # if reduce or accumulate we need all i's from 1 to the max i
  if (is_redu || is_accu) {
    i <- seq_len(max(i) + has_init)
  }

  i
}
