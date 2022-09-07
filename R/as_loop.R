#' Translate purrr's iterator functions to regular for loops
#'
#' @description
#' `as_loop()` takes a function call to one of `{purrr}`'s iterator functions, such as [purrr::map()],
#' and translates it into a regular `for` loop. Depending on the output context, the translation is
#' either (i) printed to the console, (ii) copied to the clipboard or (iii) directly inserted into
#' RStudio. Note that the latter two options require the `{clipr}` respectively the `{rstudioapi}`
#' package to be installed.
#'
#' The usage is pretty straight-forward: Just wrap a call to a `{purrr}` iterator function into
#' `as_loop()` or us one of the pipe operators (`|>` or `%>%`) to pipe the function call into
#' `as_loop()`. For details see the examples below.
#'
#' @param .expr A function call to a `{purrr}` iterator function. See the "Supported functions"
#' section below for an overview of which `{purrr}` iterator functions are currently supported.
#'
#' @param simplify When TRUE, the default, `as_loop()` will run the function call in `.expr` to
#' check two things: (1) Whether the call is valid. If not, an error will be thrown, pointing out
#' that the underlying function call is invalid. (2) Whether the resulting return value contains
#' `NULL`. In this case the `for` loop needs to be more verbose. When `simplify` is set `FALSE`
#' the function call in `.expr` is not checked for errors and the resulting for loop will be more
#' verbose even if `NULL` is not among the return values. It is recommended to set `simplify` to
#' `FALSE` for calculation-heavy function calls.
#'
#' @param output_nm sets the name of the resulting output object. The default name is `out`.
#'
#' @param idx sets the name of the index of the for loop. The default index is `i`.
#'
#' @param output_context An optional output context that defines the output target. Possible values
#' are one or several of:
#'
#'   - `"rstudio"`: This will insert the translation to the location where `as_loop()` was run. If it
#'   was run from within an R script, the for loop will be inserted there, otherwise in the console.
#'   Note that the `{rstudioapi}` package is required for this option.
#'   - `"clipboard"`: This will copy the for loop translation to the clipboard. Note that the
#'   `{clipr}` package is required for this option.
#'   - `"console"`: This will print the call to the console using `cat()`.
#'
#' The default setting is to call `default_context()`. This function first looks at the
#' `"loopurrr.output"` option. If the option is not specified, then it will default to
#' `c("rstudio", "clipboard", "console")`. In this case `as_loop()` will run the output
#' options from left to right (starting with `"rstudio"`) until successful. If neither the
#' {rstudioapi} package nor the {clipr} package are installed, the output context will fall back
#' to `"console"`.
#'
#' @param return When set to `"string"`, the default, `as_loop()` will return the translated code as
#' character strings to the location specified in `output_context`. When set to `"eval"`, the
#' translated code will be evaluated in a dedicated environment and the output object will be
#' returned. This option is especially for testing whether `as_loop()` works as expected. It should
#' be irrelevant for most users.
#'
#' @returns
#' Depending on the `return` argument the return value is:
#'  1. When `output = "string"`: `NULL`. As a side-effect, the translated for loop will be
#'  returned to the specified output context.
#'  1. When `output = "eval"`: Usually the return value of the output object that is constructed
#'  with the for loop. In case of a call to `walk`, `walk2` etc. the (first) input object will be
#'  returned.
#'
#' @section Supported functions:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' The following iterator functions from the `{purrr}` package are currently supported:
#' ```{r, comment = "#>", collapse = TRUE, eval = TRUE}
#' options(width = 60)
#' get_supported_fns("as_loop")
#' ```
#'
#' @section Examples:
#'
#' If we wrap or pipe a call to `purrr::map()` into `as_loop()` it will be translated into a
#' regular for loop. Depending on the output context, the resulting for loop will either be
#' (i) inserted directly into a script in RStudio, (ii) copied to the clipboard or (iii)
#' printed to the console.
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' x <- list(1, c(1:2), c(1:3))
#' as_loop(map(x, sum))        # wrap a call in `as_loop()`
#' map(x, sum) %>% as_loop() # pipe a call into `as_loop()`
#'
#' # --- convert: `map(x, sum)` as loop --- #
#' out <- vector("list", length = length(x))
#'
#' for (i in seq_along(x)) {
#'   out[[i]] <- sum(x[[i]])
#' }
#' # --- end loop --- #
#' ```
#'
#' The `output_nm` argument lets us specify the name of the resulting output object. In the
#' example below `".res"`. The `idx` argument lets us specify the index to be used. In the example
#' below `"j"`.
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' x <- list(1, c(1:2), c(1:3))
#' map_dbl(x, sum) %>%
#'   as_loop(., output_nm = ".res", idx = "j")
#'
#' # --- convert: `map_dbl(x, sum)` as loop --- #
#' .res <- vector("double", length = length(x))
#'
#' for (j in seq_along(x)) {
#'   .res[[j]] <- sum(x[[j]])
#' }
#' # --- end loop --- #
#' ```
#'
#' When `simplify` is set `FALSE` `as_loop` will neither check the validity of the underlying call
#' nor the expected output. In this case the resulting `for` loop is more verbose. This is because we
#' need to take the case of `NULL` in the return values into account. In the example below we further
#' see what happens, when we use an unnamed object, such as `1:3`, in the call to `purrr::map()`.
#' `as_loop()` assigns unnamed objects an internal name. In the example below `.inp1`.
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' map(1:3, sum) %>% as_loop(., simplify = FALSE)
#'
#' # --- convert: `map(1:3, sum)` as loop --- #
#' .inp1 <- 1:3
#' out <- vector("list", length = length(.inp1))
#'
#' for (i in seq_along(.inp1)) {
#'   .tmp <- sum(.inp1[[i]])
#'   if (!is.null(.tmp))
#'     out[[i]] <- .tmp
#' }
#' # --- end loop --- #
#' ```
#'
#' @export
as_loop <- function(.expr,
                    checks = TRUE, # TODO: add tests // add docs
                    force = c("auto", "yes", "no"), # TODO: add tests // add docs
                    null = c("auto", "yes", "no"), # TODO: add tests // add docs
                    output_nm = "out", # TODO: add tests
                    idx = "i", # TODO: add tests
                    output_context = default_context(),
                    return = c("string", "eval")) {

  force <- match.arg(force)
  null <- match.arg(null)
  return <- match.arg(return)
  output_context <- match.arg(output_context, several.ok = TRUE)

  if (idx == output_nm) {
    rlang::abort(
      c("Problem with `as_loop()` input `.expr`.",
        x = paste0('Index `idx` must not have the same variable name as output: "', output_nm, '".'),
        i = "Please rename `as_loop()`'s `output_nm` or `idx` argument.")
    )
  }

  q <- rlang::enquo(.expr)

  # check: magrittr pipe expression
  q <- unpipe_expr(q,
                   sc = sys.calls(),
                   is_dot = match.call()$`.expr` == ".",
                   calling_fn = "as_loop")

  # basic setup
  q_expr <- rlang::quo_get_expr(q)
  cl_chr <- call_as_chr(q_expr)

  map_fn_chr <- deparse_and_rm_nmspace(q_expr[[1]])

  map_fn <- mabye_check_map_fn(map_fn_chr, "as_loop", checks)

  output_fn <- create_output_fn(output_context, return)

  expr_ls <- reformat_expr_ls(q_expr = q_expr, fn = map_fn)

  q_env <- rlang::quo_get_env(q)

  # basic arguments
  fn_expr   <- expr_ls[[".f"]]
  x_arg     <- expr_ls[[".x"]]
  y_arg     <- expr_ls[[".y"]]
  init      <- expr_ls[[".init"]]
  at_idx    <- expr_ls[[".at"]]
  p_fn      <- expr_ls[[".p"]]
  else_fn   <- expr_ls[[".else"]]
  id_arg    <- expr_ls[[".id"]]
  dir       <- expr_ls[[".dir"]]
  def       <- expr_ls[[".default"]]
  l_arg     <- expr_ls[[".l"]]

  is <- extract_is_args(map_fn_chr, dir, fn_expr, q_env)

  returns_null <- if (is$extr_fn || null == "yes") TRUE else FALSE

  has <- extract_has_args(returns_null, is$redu, is$lmap, is$extr_fn)

  # even if force == "yes" it doesn't make sense for extractor functions
  force_eval <- if (!is$extr_fn  && force == "yes") TRUE else FALSE

  yields_error <- NULL

  # try purrr call, hide print output, check if result contains NULL:
  check_and_try_call(checks,
                     null,
                     force,
                     q,
                     map_fn_chr,
                     q_env,
                     args_ls = expr_ls[-1])

  # call dependent setup ---

  # define input list
  inp_ls <- create_inp_ls(fn_expr, l_arg, x_arg, y_arg, is$extr_fn)

  # function and arguments
  map_fn_fmls <- if (checks) rlang::fn_fmls_names(map_fn) else names(expr_ls)
  non_dot_args <- map_fn_fmls[map_fn_fmls != "..."]

  all_args <- expr_ls[-1]
  dot_args <- all_args[!names(all_args) %in% non_dot_args]

  # object and input object calls and names
  obj_nms <- NULL
  obj <- NULL
  bare_inp_nms <- NULL
  inp_objs <- create_inp_objs(inp_ls,
                              output_nm,
                              idx,
                              is_modify = is$modify,
                              is_i = is$i,
                              is_accu = is$accu,
                              is_redu = is$redu,
                              is_back = is$back,
                              q_env = q_env)

  var_nms <- create_var_nms(has$at, has$p, has$tmp, bare_inp_nms, is$lmap, is$i, cl_chr, output_nm)

  maybe_custom_inpts <- create_custom_inpts(inp_objs)

  brk <- set_brackets(is$lmap)

  # rewrite function for use in for loop
  apply_fn <- rewrite_fn(fn_expr,
                         .inp_objs = names(inp_objs),
                         .idx = idx,
                         output_nm = output_nm,
                         var_nms = var_nms,
                         fn_env = q_env,
                         cl_chr = cl_chr,
                         .brk = brk,
                         .dot_args = dot_args,
                         is_accu = is$accu,
                         has_init = has$init,
                         is_back = is$back,
                         is_redu = is$redu,
                         has_sel = has$p,
                         has_at = has$at)

  maybe_lmap_stop <- NULL

  maybe_at <- add_at(map_fn  = map_fn_chr,
                     obj     = obj,
                     output_nm = output_nm,
                     idx     = idx,
                     at      = at_idx,
                     fn_env  = q_env
                     )

  maybe_if <- add_if(map_fn  = map_fn_chr,
                     obj     = names(inp_objs),
                     output_nm = output_nm,
                     idx     = idx,
                     p_fn    = p_fn,
                     else_fn = else_fn,
                     brk     = brk,
                     fn_env  = q_env,
                     cl_chr  = cl_chr,
                     var_nms = var_nms)

  maybe_output <- create_out_obj(map_fn_chr, obj, output_nm, has$init, init, is$back)

  maybe_accu <- prep_accu_out(map_fn_chr, obj, output_nm, init, has$init, is$back)

  maybe_assign <- create_assign(map_fn_chr, output_nm, obj, idx, is$accu, has$init, is$back, is$redu)

  maybe_bind_rows_cols <- bind_rows_cols(map_fn_chr, output_nm, id_arg)

  maybe_name_obj <- create_obj_names(obj, output_nm, obj_nms, is$lmap, is$modify, is$walk, is$accu, has$init, is$back)

  maybe_post_process <- post_process(obj, q_env, output_nm, is$lmap, is$accu, is$accu2)

  maybe_return_null <- create_null_return(maybe_assign, returns_null, is$redu, is$lmap, is$extr_fn, def)

  maybe_if_selector <- create_if_selector(obj, maybe_if, else_fn, is$lmap)

  forloop_start <- create_loop_start(idx, obj, maybe_at, is$back, is$lmap, is$redu, is$accu, has$init)

  maybe_at_nonselected <- create_at_nonselected(maybe_at, is$lmap, output_nm, obj)

  maybe_if_nonselected <- create_if_nonselected(maybe_if, else_fn, is$lmap, output_nm, obj)

  maybe_assign <- create_tmp_output(maybe_assign, returns_null, is$redu, is$lmap, is$extr_fn)

  # FIXME: add this to rewrite_fn
  if (is$lmap && !is.null(maybe_at)) {
    apply_fn <- paste0('if (.sel[[', idx, ']]) {\n',
                       apply_fn,'\n',
                       '} else {\n',
                       obj,'[', idx, ']\n',
                       '}\n')
  }

  # TODO: rewrite with
  if (force_eval) {
    apply_fn <- paste0('local({\n',
                       idx, ' <- ', idx, '\n',
                       apply_fn, '\n',
                       '})')
  }

  maybe_lmap_stop <- create_lmap_stop(is$lmap, output_nm, idx)

  maybe_error <- create_warning(yields_error)

  str_out <- paste0('# --- convert: ', cl_chr, ' as loop --- #\n',
                    maybe_error,
                    maybe_custom_inpts,
                    maybe_at,
                    maybe_if_selector,
                    maybe_output,
                    maybe_accu,
                    forloop_start,
                    maybe_if,
                    maybe_assign,
                    apply_fn, '\n',
                    maybe_return_null,
                    maybe_lmap_stop,
                    '}\n',
                    maybe_at_nonselected,
                    maybe_if_nonselected,
                    maybe_name_obj,
                    maybe_bind_rows_cols,
                    maybe_post_process,
                    '# --- end loop --- #\n')

  if (return != "string") {
    str_eval <- paste0(str_out, if (is$walk) {
      paste0('\n', 'invisible(', obj, ')')
      } else {
        paste0('\n', output_nm)
      })
    out_code <- parse(text = str_eval, keep.source = FALSE)
    if (return == "eval") {
      return(eval(out_code, envir = rlang::new_environment(parent = q_env)))
    } else {
      # just in case we want `as_loop` to return a list of calls someday (not supported yet)
      return(out_code)
    }
  } else {
    output_fn(str_out)
  }
}
