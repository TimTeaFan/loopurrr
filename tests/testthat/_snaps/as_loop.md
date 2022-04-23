# as_loop does not throw an error when used on non-working map functions

    # --- convert: `map(x, log)` as loop --- #
    # --- WARNING: error detected in the call above --- #
    out <- vector("list", length = length(x))
    
    for (i in seq_along(x)) {
    out[[i]] <- log(x[[i]])
    }
    # --- end loop --- #

