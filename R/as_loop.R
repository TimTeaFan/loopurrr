
# TODO:
# reduce
# accumulate

as_loop <- function(.expr, output = default_output(), eval = FALSE, idx = "i", output_nm = "out") {

  # checks
  check_magrittr_pipe()
  # TODO: Add check
  # error: output_nm may not be `.at` or `.sel` or `.inp`
  # warn: output_nm and `.at`, `.sel` `.inp` should not exist in global environment

  output <- match.arg(output, several.ok = TRUE)

  if (eval) {
    output_fn <- NULL
  } else {
    output_fn <- create_output_fn(output)
  }

  # basic setup
  q <- rlang::enquo(.expr)
  q_expr <- rlang::quo_get_expr(q)
  cl_chr <- call_as_chr(q_expr)
  map_fn_chr <- as.character(q_expr[[1]])
  map_fn <- get(map_fn_chr, envir = rlang::as_environment("purrr"))

  q_ex_std <- rlang::call_match(call = q_expr, fn = map_fn)
  expr_ls <- as.list(q_ex_std)

  q_env <- rlang::quo_get_env(q)


  is_supported(map_fn_chr)

  # put these calls in a setup function
  is_lmap <- grepl("^lmap", map_fn_chr, perl = TRUE)
  is_walk <- grepl("^(walk|iwalk|pwalk)", map_fn_chr, perl = TRUE)
  is_i <- grepl("(^imap)|(^iwalk)|(^imodify)", map_fn_chr, perl = TRUE)
  is_modify <- grepl("modify", map_fn_chr, perl = TRUE)
  is_accu <- grepl("accumulate", map_fn_chr, perl = TRUE)

  # call dependent setup ---

  # define input list
  if (!is.null(expr_ls[[".l"]])) {
    inp_ls <- as.list(expr_ls[[".l"]][-1])
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
  # FIXME: Will this work with modify2 ???!
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
  if (is_accu) {
    inp_objs <- purrr::prepend(inp_objs,
                               rlang::list2("{output_nm}" := output_nm))
  }

  custom_inp_objs <- inp_objs[names(inp_objs) != inp_objs]
  maybe_custom_inpts <- create_custom_inpts(custom_inp_objs)

  if (is_lmap) {
    brk <- list(o = '[',
                c = ']')
  } else {
    brk <- list(o = '[[',
                c = ']]')
  }

  # call dependent implementation
  init     <- expr_ls[[".init"]]
  has_init <- !is.null(init)
  apply_fn <- rewrite_fn(expr_ls[[".f"]], names(inp_objs), idx, q_env, cl_chr, brk, dot_args, is_accu, has_init)
  at_idx   <- expr_ls[[".at"]]
  p_fn     <- expr_ls[[".p"]]
  else_fn  <- expr_ls[[".else"]]
  id_arg   <- expr_ls[[".id"]]
  dir      <- expr_ls[[".dir"]]


  maybe_lmap_stop <- NULL

  maybe_at <- add_at(map_fn  = map_fn_chr,
                     obj     = obj,
                     # obj_nms = obj_nms,
                     output_nm = output_nm,
                     idx     = idx,
                     at      = at_idx
                     # fn_env  = q_env,
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

  maybe_output <- create_out_obj(map_fn_chr, obj, output_nm, has_init)

  maybe_accu_redu <- prep_accu_out(map_fn_chr, obj, output_nm, init, has_init)

  maybe_assign <- create_assign(map_fn_chr, output_nm, obj, idx, is_accu, has_init)

  maybe_bind_rows_cols <- bind_rows_cols(map_fn_chr, output_nm, id_arg)

  maybe_name_obj <- create_obj_names(obj, output_nm, obj_nms, is_lmap, is_modify, is_walk, is_accu, has_init)

  maybe_flatten_tbl <- finish_lmap(is_lmap, obj, q_env, output_nm)

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
                    if (!is.null(maybe_if) && is.null(else_fn) && !is_lmap) paste0('.sel <- vector("logical", length = length(', obj,'))\n'),
                    # maybe_init,
                    maybe_output,
                    maybe_accu_redu,
                    paste0('\nfor (',idx,' in seq_along(', obj,')'),
                    if (!is.null(maybe_at) && !is_lmap) '[.sel]',
                    if (is_accu && !has_init) '[-1]',
                    ') {\n',
                    maybe_if,
                    maybe_assign,
                    apply_fn, '\n',
                    maybe_lmap_stop,
                    '}\n',
                    if (!is.null(maybe_at) && !is_lmap) paste0('\n', output_nm, '[-.sel] <- ', obj,'[-.sel]\n'),
                    if (!is.null(maybe_if) && is.null(else_fn) && !is_lmap) paste0('\n', output_nm, '[.sel] <- ', obj,'[.sel]\n'),
                    maybe_name_obj,
                    maybe_bind_rows_cols,
                    maybe_flatten_tbl,
                    '# --- end loop --- #\n')

  if (eval) {
    str_eval <- paste0(str_out, if(!is_walk) {paste0('\n', output_nm)})
    eval(parse(text = str_eval, keep.source = FALSE), envir = rlang::new_environment(parent = q_env))
  } else {
    output_fn(str_out)
  }
}
