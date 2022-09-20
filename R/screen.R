screen <- function(.expr, ...) {

  # browser()
  dots <- rlang::list2(...)

  # checks if screen is used to implement another function
  implement <- !is.null(dots[[".__impl__."]])

  if (implement) {
    q <- .expr
  } else {
    q <- rlang::enquo(.expr)

    # check and unpipe
    q <- unpipe_expr(q,
                     sc = sys.calls(),
                     is_dot = match.call()$`.expr` == ".",
                     calling_fn = "screen")
  }

  q_expr <- rlang::quo_get_expr(q)
  q_env <- rlang::quo_get_env(q)

  map_fn_chr <- deparse(q_expr[[1]])

  if (grepl("^\\w+::", map_fn_chr, perl = TRUE)) {
    map_fn_chr <- gsub("^\\w+::", "", map_fn_chr)
  }

  map_fn <- get(map_fn_chr, envir = rlang::as_environment("purrr"))
  q_ex_std <- match.call(definition = map_fn, call = q_expr)

  cl_chr <- call_as_chr(q_expr)

  expr_ls <- as.list(q_ex_std)

  obj_x <- eval(expr_ls[[".x"]], envir = q_env)

  res <- wrap(.expr = q,
              .f = function(x) quietly(safely(x)),
              silent = TRUE,
              `.__impl__.` = TRUE
  )

  results_ls  <- map(res, c("result", "result"), .default = NA)
  errors_ls   <- map(res, c("result", "error"), .default = NA)
  output_ls   <- map(res, c("output"), .default = NA)
  warnings_ls <- map(res, c("messages"), .default = NA)
  message_ls  <- map(res, c("warnings"), .default = NA)

  errors_ls <- map(errors_ls, .f = get_error_msg)

  out <- tibble::tibble(input = obj_x,
                        inp_class = purrr::map(obj_x, class),
                        result = results_ls,
                        res_class = purrr::map(results_ls, class),
                        error = errors_ls,
                        output = output_ls,
                        warnings = warnings_ls,
                        message = message_ls)

  out2 <- dplyr::mutate(out,
                        across(everything(),
                               unlist_if_all_length_one)
  )

  class(out2) <- c("screen_tbl", class(out2))
  attr(out2, "call") <- cl_chr
  out2
}

print.screen_tbl <- function(x) {
  ln <- nrow(x)
  no_err  <- length(na.omit(x$error))
  no_warn <- length(na.omit(x$warnings))
  res_classes <- na.omit(unique(x$res_class[!is.na(x$result)]))
  no_of_classes <- length(res_classes)

  if ( no_of_classes > 0) {
    class_res    <- purrr::imap(res_classes, ~ paste0("[", .y, "] ", .x))
    class_output <- paste(class_res, collapse = ", ")
    if (nchar(class_output) > 72) {
      class_output <- paste0(substr(class_output, 1, 75), " ...")
    }
  }

  cat(paste0("# No. of errors: ",   no_err,  " (", round((no_err/ln)  * 100, 0), "%)", "\n"))
  cat(paste0("# No. of warnings: ", no_warn, " (", round((no_warn/ln) * 100, 0), "%)", "\n"))
  cat(paste0("# No. of result classes: ", no_of_classes), "\n")
  if (no_of_classes > 0) {
    cat(paste0("# ", trimws(class_output, "right"), ".\n"))
  }
  NextMethod()
}

unlist_if_all_length_one <- function(x) {
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
