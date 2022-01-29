#' Translate {purrr} iterative functions to regular for loops
#'
#' @description
#' `as_loop()` takes a function call to one of {purrr}'s iterator functions, such as [purrr::map()],
#' and translates it into a regular `for`-loop. Depending on the output context, the translation is
#' either (i) printed to the console, (ii) copied to the clipboard or (iii) directly inserted into
#' RStudio. Note that For the latter two options the {clipr} respectively the {rstudioapi} packages
#' are required.
#'
#' The usage is pretty straight-forward: Just wrap a call to a {purrr} iterator function into
#' `as_loop()` or us one of the pipe operators (`|>` or `%>%`) to pipe the function call into
#' `as_loop()`. For details see the examples below.
#'
#' @param .expr A function call to a {purrr} iterator function. See the "Supported functions"
#' section below for an overview of which {purrr} iterator functions are currently supported.
#'
#' @param simplify When TRUE, the default, `as_loop()` will run the function call in `.expr` to
#' check two things: (1) Whether the call is valid. If not, an error will be thrown, pointing out
#' that the underlying function call is invalid. (2) Whether the resulting return value contains
#' `NULL`. In this case the the for loop needs to be more verbose. When `simplify` is set `FALSE`
#' the function call in `.expr` is not checked for errors and resulting for loop will be more verbose
#' even if `NULL` is not among the return values. It is recommened to set `simplify` to `FALSE` for
#' calculation-heavy function calls.
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
#'   Note that the {rstudioapi} package is needed for this option.
#'   - `"clipboard"`: This will copy the for loop translation to the clipboard. Note that the
#'   {rstudioapi} package is needed for this option.
#'   - `"console"`: This will print the call to the console using `cat()`.
#'
#' The default setting is to call `default_context()`. This function first looks at the
#' `"loopurrr.output"` option. If the option is not specified, then it will default to
#' `c("rstudio", "clipboard", "console")`. In this case `as_loop()` will try each output
#' option starting with `"rstudio"`. If neither the {rstudioapi} package nor the {clipr}
#' package are installed, the output context will fall back to the `"console"`.
#'
#' @param return When set to `"string"`, the default, `as_loop()` will return the translated code as
#' character strings to the location specified in `output_context`. When set to `"eval"`, the
#' translated code will be evaluated in a dedicated environment and the output object will be
#' returned. This option is especially for testing whether `as_loop()` works as expected. It should
#' be irrelevant for most users.
#'
#' @returns
#' Depending on the `return` argument the return value is:
#'  1. When `output == "string"`: `NULL`. As a side-effect, the translated for loop will be
#'  returned to the specified output context.
#'  1. When `output == "eval"`: Usually the return value of the output object that is constructed
#'  with the for loop. In case of a call to `walk`, `walk2` etc. the (first) input object will be
#'  returned.
#'
#' @section Supported functions:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' The following iterator functions from the {purrr} package are currently supported:
#' ```{r, comment = "#>", collapse = TRUE, echo = FALSE, eval = TRUE}
#' dplyr::tibble(id = c("map_", "imap", "map2", "pmap", "modify", "i?walk", "accumulate", "reduce")) %>%
#'  dplyr::rowwise() %>%
#'  dplyr::mutate(fns = list(grep(paste0("^", id), get_supported_fns(), value = TRUE))) %>%
#'  dplyr::ungroup() %>%
#'  dplyr::mutate(fns = purrr::set_names(fns, id)) %>%
#'  dplyr::pull(fns)
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
#' as_loop(map(1:3, sum))      # wrap a call in `as_loop`
#' map(1:3, sum) %>% as_loop() # pipe a call into `as_loop`
#'
#' # --- convert: `map(1:3, sum)` as loop --- #
#' .inp1 <- 1:3
#' out <- vector("list", length = length(.inp1))
#'
#' for (i in seq_along(.inp1)) {
#'   out[[i]] <- sum(.inp1[[i]])
#' }
#' # --- end loop --- #
#' ```
#'
#' The `output_nm` argument lets us specify the name of the resulting output object. In the
#' example below `".res"`. The `idx` argument lets us specify the index to be used. In the example
#' below `"j"`.
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
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
#' nor the expected output. In this case the resulting for loop is more verbose to account for the
#' case of `NULL` in the return values.
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
# --- end loop --- #
#' ```
#'
#' @export
as_loop <- function(.expr,
                    simplify = TRUE,
                    output_nm = "out",
                    idx = "i",
                    output_context = default_context(),
                    return = c("string", "eval")) {

  return <- match.arg(return, several.ok = FALSE)
  output_context <- match.arg(output_context, several.ok = TRUE)

  q <- rlang::enquo(.expr)

  # TODO: wrap this block into one function
  # check: magrittr pipe expression
  new_expr <- check_and_unpipe(sys.calls(), is_dot = match.call()$`.expr` == ".")
  if (!is.null(new_expr)) {
    q <- rlang::quo_set_expr(q, new_expr)
  }

  # basic setup
  q_expr <- rlang::quo_get_expr(q)
  cl_chr <- call_as_chr(q_expr)

  # TODO: deparse and remove namespace into function:
  map_fn_chr <- deparse(q_expr[[1]])
  # remove namespace
  if(grepl("^\\w+::", map_fn_chr, perl = TRUE)) {
    map_fn_chr <- gsub("^\\w+::", "", map_fn_chr)
  }

  is_supported(map_fn_chr)
  map_fn <- get(map_fn_chr, envir = rlang::as_environment("purrr"))


  # TODO: Add check
  # error: output_nm may not be `.at` or `.sel` or `.inp`
  # warn: output_nm and `.at`, `.sel` `.inp` should not exist in global environment


  if (return == "eval") {
    output_fn <- NULL
  } else {
    output_fn <- create_output_fn(output_context)
  }

  q_ex_std <- rlang::call_match(call = q_expr, fn = map_fn)
  expr_ls <- as.list(q_ex_std)
  q_env <- rlang::quo_get_env(q)

  # put these calls in a setup function
  fn_expr   <- expr_ls[[".f"]]
  init      <- expr_ls[[".init"]]
  at_idx    <- expr_ls[[".at"]]
  p_fn      <- expr_ls[[".p"]]
  else_fn   <- expr_ls[[".else"]]
  id_arg    <- expr_ls[[".id"]]
  dir       <- expr_ls[[".dir"]]
  def       <- expr_ls[[".default"]]

  # put these calls in another setup function
  has_init   <- !is.null(init)
  is_back    <- !is.null(dir) && dir == "backward"
  is_lmap    <- grepl("^lmap", map_fn_chr, perl = TRUE)
  is_walk    <- grepl("^(walk|iwalk|pwalk)", map_fn_chr, perl = TRUE)
  is_i       <- grepl("(^imap)|(^iwalk)|(^imodify)", map_fn_chr, perl = TRUE)
  is_modify  <- grepl("modify", map_fn_chr, perl = TRUE)
  is_accu    <- grepl("accumulate", map_fn_chr, perl = TRUE)
  is_accu2   <- grepl("^accumulate2$", map_fn_chr, perl = TRUE)
  is_redu    <- grepl("reduce", map_fn_chr, perl = TRUE)
  is_extr_fn <- check_extr_fn(fn_expr, q_env)

  returns_null <- if(is_extr_fn || !simplify) TRUE else FALSE

  # try purrr call, hide print output, check if result contains NULL:
  if (simplify) {
    res <- try_purr_call(q, map_fn_chr)
    returns_null <- any(purrr::map_lgl(res, is.null))
  }

  # call dependent setup ---

  # wrap this logic into a function that return `inp_ls
  # define input list
  if (!is.null(expr_ls[[".l"]])) {
    inp_ls <- as.list(expr_ls[[".l"]][-1])
  } else if (is_extr_fn) {
    inp_ls <- list(.x = expr_ls[[".x"]],
                   .y = fn_expr)
  } else {
    inp_ls <- list(.x = expr_ls[[".x"]],
                   .y = expr_ls[[".y"]])
  }

  # function and arguments
  map_fn_fmls <- rlang::fn_fmls_names(map_fn)
  non_dot_args <- map_fn_fmls[map_fn_fmls != "..."]

  all_args <- expr_ls[-1]
  dot_args <- all_args[!names(all_args) %in% non_dot_args]

  # object and input object calls and names
  inp_objs <- create_inp_objs(inp_ls)

  # TODO:  add this to `create_inp_objs`
  if (!is.null(inp_objs) && is_modify) {
    names(inp_objs)[1] <- output_nm
  }
  obj_nms <- get_obj_names(inp_objs[1], q_env)
  obj <- names(inp_objs)[1]

  # if imap
  if (is_i) {
    inp_objs <- append(inp_objs,
                       list(.idx = names_or_idx(obj, obj_nms)))
  }
  if (is_accu || is_redu) {
    inp_objs <- if(!is_back) {
      purrr::prepend(inp_objs,
                     rlang::list2("{output_nm}" := output_nm))
    } else {
      append(inp_objs,
             rlang::list2("{output_nm}" := output_nm),
             after = 1)
    }
  }
  # add all the above to `create_inp_objs` and let it return list with multiple outputs

  # TODO: add this line to `create_custom_inpts`:
  custom_inp_objs <- inp_objs[names(inp_objs) != inp_objs]
  maybe_custom_inpts <- create_custom_inpts(custom_inp_objs)

  # TODO: wrap into funciton
  if (is_lmap) {
    brk <- list(o = '[',
                c = ']')
  } else {
    brk <- list(o = '[[',
                c = ']]')
  }

  apply_fn <- rewrite_fn(fn_expr,
                         names(inp_objs),
                         idx,
                         q_env,
                         cl_chr,
                         brk,
                         dot_args,
                         is_accu,
                         has_init,
                         is_back,
                         is_redu)


  maybe_lmap_stop <- NULL

  maybe_at <- add_at(map_fn  = map_fn_chr,
                     obj     = obj,
                     output_nm = output_nm,
                     idx     = idx,
                     at      = at_idx,
                     fn_env  = q_env
                     )

  maybe_if <- add_if(map_fn  = map_fn_chr,
                     obj     = obj,
                     output_nm = output_nm,
                     idx     = idx,
                     p_fn    = p_fn,
                     else_fn = else_fn,
                     brk     = brk,
                     fn_env  = q_env
                     )

  maybe_output <- create_out_obj(map_fn_chr, obj, output_nm, has_init, init, is_back)

  maybe_accu <- prep_accu_out(map_fn_chr, obj, output_nm, init, has_init, is_back)

  maybe_assign <- create_assign(map_fn_chr, output_nm, obj, idx, is_accu, has_init, is_back, is_redu)

  maybe_bind_rows_cols <- bind_rows_cols(map_fn_chr, output_nm, id_arg)

  maybe_name_obj <- create_obj_names(obj, output_nm, obj_nms, is_lmap, is_modify, is_walk, is_accu, has_init, is_back)

  maybe_post_process <- post_process(obj, q_env, output_nm, is_lmap, is_accu, is_accu2)

  maybe_return_null <- create_null_return(maybe_assign, returns_null, is_redu, is_lmap, is_extr_fn, def)

  # TODO: wrap those parts in functions:
  maybe_if_selector <- if (!is.null(maybe_if) && is.null(else_fn) && !is_lmap) paste0('.sel <- vector("logical", length = length(', obj,'))\n')

  forloop_start <- paste0('\nfor (',idx,' in ', if(is_back) 'rev(', 'seq_along(', obj, ')', if(is_back) ')')

  maybe_at_nonselected <- if (!is.null(maybe_at) && !is_lmap) paste0('\n', output_nm, '[-.sel] <- ', obj,'[-.sel]\n')

  maybe_if_nonselected <- if (!is.null(maybe_if) && is.null(else_fn) && !is_lmap) paste0('\n', output_nm, '[.sel] <- ', obj,'[.sel]\n')

  maybe_null_or_extractor_fn <- if((returns_null && !is_redu && !is_lmap) || is_extr_fn) '.tmp <-' else maybe_assign

  # FIXME: add this to rewrite_fn
  if (is_lmap && !is.null(maybe_at)) {
    apply_fn <- paste0('if (.sel[[', idx, ']]) {\n',
                       apply_fn,'\n',
                       '} else {\n',
                       obj,'[', idx, ']\n',
                       '}\n')
  }

  maybe_lmap_stop <- if (is_lmap) {
    paste0('stopifnot(is.list(', output_nm,'[[', idx, ']]))\n')
    } else NULL


  str_out <- paste0('# --- convert: ', cl_chr, ' as loop --- #\n',
                    maybe_custom_inpts,
                    maybe_at,
                    maybe_if_selector,
                    maybe_output,
                    maybe_accu,
                    forloop_start,
                    # this part to forloop_start
                    if (!is.null(maybe_at) && !is_lmap) '[.sel]',
                    if ((is_redu || is_accu) && !has_init) '[-1]',
                    ') {\n',
                    # the part above to forloop_start
                    maybe_if,
                    maybe_null_or_extractor_fn,
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
    str_eval <- paste0(str_out, if(is_walk) {
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
