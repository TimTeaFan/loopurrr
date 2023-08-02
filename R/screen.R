screen <- function(.expr, ...) {

  # browser()
  dots <- rlang::list2(...)

  # checks if screen is used to implement another function
  implement <- !is.null(dots[[".__impl__."]])

  # the hidden args should not be part of the dots
  dots <- dots[-which(names(dots) %in% c(".__impl__."))]

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
                        warning = warnings_ls,
                        message = message_ls)

  # TODO: add rows based on `...` with following arguments:
  # .x, .y,

  out2 <- dplyr::mutate(out,
                        across(everything(),
                               unlist_if_all_length_one)
  )

  attach_screen_attr(out2, cl_chr)
}

attach_screen_attr <- function(x, cl_chr) {

  ln <- nrow(x)

  attr(x, "class") <- c("screen_tbl", attr(x, "class"))
  attr(x, "call") <- cl_chr

  attr(x, "no_err")  <- no_err <- length(na.omit(x$error))
  attr(x, "perc_err") <- round((no_err/ln)  * 100, 0)

  attr(x, "no_warn") <- no_warn <- length(na.omit(x$warning))
  attr(x, "perc_warn") <- round((no_warn/ln)  * 100, 0)

  res_classes <- na.omit(unique(x$res_class[!is.na(x$result)]))
  attr(x, "no_of_classes") <- no_of_classes <- length(res_classes)

  if ( no_of_classes > 0) {
    class_res    <- purrr::imap(res_classes, ~ paste0("[", .y, "] ", .x))
    class_output <- paste(class_res, collapse = ", ")
    if (nchar(class_output) > 72) {
      class_output <- paste0(substr(class_output, 1, 75), " ...")
    }
    attr(x, "class_output") <- class_output
  }
  x
}


summary.screen_tbl <- function(x) {

  screen_attr <- c("call", "no_err", "perc_err", "no_warn", "perc_warn",
                   "no_of_classes", "class_output")

  attr_x <- attributes(x)[screen_attr]

  grp_dat <- dplyr::group_by(x, inp_class, res_class, error, warning)

  res <- dplyr::summarise(grp_dat,
                          idx_ls = list(input),
                          idx = paste(input, collapse = ", ")
                          )

  res2 <- dplyr::mutate(res,
                        type = dplyr::case_when(
                          is.na(error) & is.na(warning) ~ "result",
                          !is.na(error) ~ "error",
                          !is.na(warning) ~ "warning"
                          ),
                        .before = 1L)

  nms <- make.names(res2$type, unique = TRUE)
  res$idx_ls <- set_names(res$idx_ls, nms)

  attributes(res) <- c(attributes(res), attr_x)
  class(res) <- c("screen_tbl", class(res))

  res
}

print.screen_tbl <- function(x) {
  cl_chr <- attr(x, "call")
  no_err  <- attr(x, "no_err")
  perc_err <- attr(x, "perc_err")
  no_warn <- attr(x, "no_warn")
  perc_warn <- attr(x, "perc_warn")
  res_classes <-  attr(x, "res_classes")
  no_of_classes <-  attr(x, "no_of_classes")
  class_output <- attr(x, "class_output")

  cat(paste_subtle("# Screen call: ", cl_chr, "\n"))
  cat(paste_subtle("# No. of errors: ",   no_err,  " (", perc_err, "%)", " || "))
  cat(paste_subtle("# No. of warnings: ", no_warn, " (", perc_warn, "%)", "\n"))
  cat(paste_subtle("# No. of result classes: ", no_of_classes), "\n")

  if (no_of_classes > 0) {
    cat(paste_subtle("# └─ ", trimws(class_output, "right"), ".\n"))
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

paste_subtle <- function(...) {
  pillar::style_subtle(paste0(...))
}
