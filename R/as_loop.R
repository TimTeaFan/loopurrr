
# TODO:
# lmap at/if
# reduce
# accumulate
# modify at/if

as_loop <- function(.expr, output_nm = "out", idx = "i", output = c("rstudio", "clipboard", "console"), eval = FALSE) {

  # checks
  check_magrittr_pipe()
  # TODO: Add check
  # error: output_nm may not be `.at` or `.sel` or `.inp`
  # warn: output_nm and `.at`, `.sel` `.inp` should not exist in global environment

  output <- match.arg(output, several.ok = TRUE)

  if (eval) {
    output_fn <- NULL
  } else {
    output_fn <- get_output_fn(output)
  }

  # basic setup
  q <- rlang::enquo(.expr)
  q_expr <- rlang::quo_get_expr(q)
  cl_chr <- call_as_chr(q_expr)

  q_ex_std <- rlang::call_standardise(q_expr)
  expr_ls <- as.list(q_ex_std)

  q_env <- rlang::quo_get_env(q)

  map_fn_chr <- as.character(expr_ls[[1]])
  is_supported(map_fn_chr)


  # call dependent setup ---

  # define input list
  if (!is.null(expr_ls[[".l"]])) {
    inp_ls <- as.list(expr_ls[[".l"]][-1])
  } else {
    inp_ls <- list(.x = expr_ls[[".x"]],
                   .y = expr_ls[[".y"]] )
  }



  # function and arguments
  map_fn <- get(map_fn_chr, envir = rlang::as_environment("purrr"))
  map_fn_fmls <- rlang::fn_fmls_names(map_fn)
  non_dot_args <- map_fn_fmls[map_fn_fmls != "..."]

  all_args <- expr_ls[-1]
  dot_args <- all_args[!names(all_args) %in% non_dot_args]

  inp_objs <- create_inp_objs(inp_ls)
  obj_nms <- get_obj_names(inp_objs[1], q_env)
  obj <- names(inp_objs)[1]

  is_lmap <- grepl("^lmap", map_fn_chr, perl = TRUE)
  is_walk <- grepl("^(walk|iwalk)", map_fn_chr, perl = TRUE)
  is_i <- grepl("(^imap)|(^iwalk)", map_fn_chr, perl = TRUE)

  # if imap
  if (is_i) {
    inp_objs <- append(inp_objs,
                       list(.idx = names_or_idx(obj, obj_nms)))
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
  apply_fn <- rewrite_fn(expr_ls[[".f"]], names(inp_objs), idx, q_env, cl_chr, brk, dot_args)
  at_idx   <- expr_ls[[".at"]]
  p_fn     <- expr_ls[[".p"]]
  else_fn  <- expr_ls[[".else"]]
  id_arg   <- expr_ls[[".id"]]

  maybe_at <-
    maybe_if <-
    maybe_output <-
    maybe_assign <-
    maybe_bind_rows_cols <-
    maybe_name_obj <-
    maybe_flatten_tbl <-
    maybe_lmap_stop <- NULL

  if (!is_walk) {

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
                       brk     = brk
                       # fn_env  = q_env,
                       )

    maybe_output <- create_out_obj(map_fn_chr, obj, output_nm)

    maybe_assign <- paste0(output_nm, '[[', idx, ']] <- ')

    maybe_bind_rows_cols <- bind_rows_cols(map_fn_chr, output_nm, id_arg)

    maybe_name_obj <- if (!is.null(obj_nms) && !is_lmap) {
      paste0('\nnames(', output_nm, ') <- names(', obj, ')\n')
    }

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
  }


  str_out <- paste0('# --- convert: ', cl_chr, ' as loop --- #\n',
                    maybe_custom_inpts,
                    maybe_at,
                    if (!is.null(maybe_if) && is.null(else_fn) && !is_lmap) paste0('.sel <- vector("logical", length = length(', obj,'))\n'),
                    maybe_output,
                    paste0('for (',idx,' in seq_along(', obj,')'),
                    if (!is.null(maybe_at) && !is_lmap) '[.sel]',
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
    eval(parse(text = str_eval, keep.source = FALSE), envir = q_env)
  } else {
    output_fn(str_out)
  }
}
