
mptformula <- function(formula, ..., response, model, brms_args = list()) {
  if (missing(model)) {
    stop("model object needs to be provided.", call. = FALSE)
  }
  dots <- list(...)
  if (length(dots) == 0) {
    formula_out <- vector("list", length(model$parameters))
    full_formula <- vector("list", length(model$parameters))
    for (i in seq_along(model$parameters)) {
      formula_out[[i]] <- formula
      if (i > 1) formula_out[[i]][[2]] <- as.name(model$parameters[i])
      full_formula[[i]] <- formula
      full_formula[[i]][[2]] <- as.name(model$parameters[i])
    }
    if (!missing(response)) message("response argument ignored.")
    response <- as.formula(paste("~", formula_out[[1]][[2]]),
                           env = globalenv())
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
        response <- as.formula(paste("~", response), env = globalenv())
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
    full_formula <- all_formulas[match(model$parameters, all_vars_formula)]
  }
  brmsformula <- do.call(what = brms::brmsformula,
          args = c(
            formula = formula_out[[1]],
            flist = list(formula_out[-1]),
            family = list(model$family),
            brms_args))
  out <- list(
    formulas = full_formula,
    response = response,
    brmsformula = brmsformula,
    model = model
  )
  class(out) <- c("mpt_formula")
  out
}

#' @export
print.mpt_formula <- function(x, ...) {
  cat("MPT formulas (response: ",
      all.vars(x$response), "):\n", sep = "")
  for (i in seq_along(x[[1]])) {
    print(x[[1]][[i]])
  }
  invisible(x)
}

#' @export
standata.mpt_formula <- function(object, data,
                                 tree,
                                 brms_args = list()) {
  data_prep <- data
  resp_char <-  parse_single_var(object$response, data, "response")
  tree_char <-  parse_single_var(tree, data, "response")
  data_prep[["mpt_original_response"]] <- data_prep[[resp_char]]
  data_split <- split(
    x = data_prep,
    f = factor(data[[tree_char]], levels = object$model$names$trees)
  )
  for (i in seq_along(data_split)) {
    data_split[[i]][[resp_char]] <- as.numeric(factor(
      x = data_split[[i]][[resp_char]],
      levels = object$model$names$categories[[i]]
    ))
    data_split[[i]][["mpt_n_categories"]] <-
      length(object$model$names$categories[[i]])
    data_split[[i]][["mpt_item_type"]] <- i
  }
  data_prep <- do.call("rbind", data_split)
  out <- do.call(brms::standata,
                 args = c(
                   object = list(object$brmsformula),
                   data = list(data_prep),
                   family = list(object$model$family),
                   brms_args
                 ))
  return(out)
}

parse_single_var <- function(x, data, argument) {
  if (inherits(x, "formula")) {
    out <- all.vars(x)
  } else if (is.character(x)) {
    out <- x
  } else {
    stop(argument, "needs to be either a character or on-sided formula",
         call. = FALSE)
  }
  if (length(out) != 1) {
    stop(argument, "needs to contain one variable only",
         call. = FALSE)
  }
  if (!(out %in% colnames(data))) {
    stop(out, "is not a variable in data", call. = FALSE)
  }
  out
}
