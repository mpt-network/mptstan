#' Set up MPT model formula
#'
#' Set up model formula(s) that specify the regression structure and
#' hierarchical structure for MPT model formulas. The regression structure allow
#' to vary parameters across both between-participants and within-participants
#' conditions. By using `lme4`/`brms` style syntax (e.g. `(1|id)`) the
#' random-effects (i.e., a hierarchical structure) can be defined.
#'
#'
#' @param formula An object of class `formula` providing a symbolic description
#'   of the regression model and hierarchical structure applied to MPT model
#'   parameters. If only one formula is given, the left-hand-side (LHS) needs to
#'   give the response variable and the right-hand-side (RHS) gives the model
#'   structure for all parameters.
#' @param ... optional additional `formula` objects providing a symbolic
#'   description of the regression model and hierarchical structure applied to
#'   the remaining MPT model parameters.
#' @param response one sided formula or character vector giving the name of the
#'   response variable, if not specified as LHS of `formula`. Cannot be missing
#'   if a `formula` for each MPT model parameter is specified.
#' @param model An `mpt_model` object as created by [make_mpt()].
#' @param brms_args A `list` of additional arguments passed to
#'   [brms::brmsformula()], such as `center`, which is the function ultimately
#'   creating the formula for fitting the model.
#'
#' @details
#' There are two ways of using this function:
#' 1. Specify a single formula that applies to all MPT model parameters (passed
#'   via `model`). In this case, the LHS of the formula needs to give the
#'   response variable.
#' 2. Specify a formula for each MPT model parameter of the `model`.
#'   In this case, the LHS of each formula needs to give the parameters name.
#'   Furthermore, the name of the response variable needs to be passed via the
#'   `response` argument.
#'
#'
#'
#' @returns An object of class `mpt_formula` which is a list containing the
#'   following slots:
#' 1. `formulas`: A `list` of formulas for each MPT model parameter.
#' 2. `response`: A one-sided `formula` given the response variable on the RHS.
#' 3. `brmsformula`: The `brmsformula` object created by [brms::brmsformula()].
#' 4. `model`: The `mpt_model` object passed in the `model` argument.
#'
#' @example examples/examples_mpt_formula.R
#'
#' @importFrom stats as.formula
#' @export
mpt_formula <- function(formula, ..., response, model, brms_args = list()) {
  if (missing(model)) {
    stop("model object needs to be provided.", call. = FALSE)
  }
  dots <- list(...)
  dots_not_formulas <- dots[!vapply(dots, FUN = inherits,
                                    FUN.VALUE = TRUE, what = "formula")]
  if (length(dots_not_formulas) > 0) {
    stop("Non-formula arguments in '...' ignored: ",
         paste(names(dots_not_formulas), collapse = ", "), call. = FALSE)
  }
  dots_formulas <- dots[vapply(dots, FUN = inherits,
                                 FUN.VALUE = TRUE, what = "formula")]

  if (length(dots_formulas) == 0) {
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
  if (length(brmsformula$pforms) > 0) {
    attr <- attributes(brmsformula$formula)
    for (i in seq_along(brmsformula$pforms)) {
      attributes(brmsformula$pforms[[i]]) <- attr
    }
  }
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
