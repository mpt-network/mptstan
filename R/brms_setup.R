
mptformula <- function(formula, ..., response, model, brms_args = list()) {
  if (missing(model)) {
    stop("model object needs to be provided.", call. = FALSE)
  }
  dots <- list(...)
  if (length(dots) == 0) {
    formula_out <- vector("list", length(model$parameters))
    for (i in seq_along(model$parameters)) {
      formula_out[[i]] <- formula
      if (i > 1) formula_out[[i]][[2]] <- as.name(model$parameters[i])
    }
    if (!missing(response)) message("response argument ignored.")
  } else {
    dots_formulas <- dots[vapply(dots, FUN = inherits,
                                 FUN.VALUE = TRUE, what = "formula")]
    all_formulas <- c(formula, dots_formulas)
    if (!all(vapply(all_formulas, length, 1L) == 3)) {
      stop("all formulas need to have LHS and RHS", call. = FALSE)
    }
    if (missing(response)) {
      stop("response cannot be missing, if individual formulas are provided.",
           call. = FALSE)
    } else {
      if (is.character(response)) {
        response <- as.formula(paste("~", response))
      }
    }
    all_vars_formula <- vapply(all_formulas,
                               function(x) as.character(x[[2]]), FUN.VALUE = "")
    if(!setequal(all_vars_formula, model$parameters)) {
      stop("all parameters need formulas. missing: ",
           setdiff(model$parameters, all_vars_formula), call. = FALSE)
    }
    formula_out <- all_formulas[match(model$parameters, all_vars_formula)]
    formula_out[[1]][[2]] <- response[[2]]
  }
  do.call(what = brms::brmsformula,
          args = c(
            formula = formula_out[[1]],
            flist = list(formula_out[-1]),
            family = list(model$family),
            brms_args))
}

