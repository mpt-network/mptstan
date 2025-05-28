#' Set up MPT model formula
#'
#' `mpt_formula()` sets up model formula(s) that specify the regression
#' structure and hierarchical structure for MPT model formulas. The regression
#' structure allows to vary parameters across both between-participants and
#' within-participants conditions. By using `lme4`/`brms` style syntax (e.g.
#' `(1|id)`) the random-effects (i.e., a hierarchical structure) can be defined.
#' For `mpt_formula` objects, [brms::stancode] and [brms::standata] methods are
#' provided.
#'
#'
#' @param formula An object of class `formula` providing a symbolic description
#'   of the regression model and hierarchical structure applied to MPT model
#'   parameters. If only one formula is given, the left-hand-side (LHS) needs to
#'   give the response variable and the right-hand-side (RHS) gives the model
#'   structure for all parameters.
#' @param ... for `mpt_formula()`, optional additional `formula` objects
#'   providing a symbolic description of the regression model and hierarchical
#'   structure applied to the remaining MPT model parameters. For the
#'   `mpt_formula` methods, additional arguments passed to the corresponding
#'   default methods.
#' @param response one sided formula or character vector giving the name of the
#'   response variable. Cannot be missing if a `formula` is specified for each
#'   parameter and the data are given in long format (see `data_format` arg)
#' @param model An `mpt_model` object as created by [make_mpt()].
#' @param data_format character string indicating whether the formula is to be
#'   generated for fitting data in long format / non-aggregated data (`long`,
#'   the default), where a single variable contains trial-level responses, or
#'   for data in wide format / aggregated data (`wide`), where a separate column
#'   for each response category contains the respective frequency.
#' @param brms_args A `list` of additional arguments passed to
#'   [brms::brmsformula()], such as `center`, which is the function ultimately
#'   creating the formula for fitting the model.

#'
#' @details
#' There are two ways of using `mpt_formula()` function:
#' 1. Specify a single formula that applies to all MPT model parameters (passed
#'   via `model`). In this case, the LHS of the formula needs to give the
#'   response variable if data is in long format (LHS is ignored if an LHS
#'   is given for a formula for data in wide format / aggregated data).
#' 2. Specify a formula for each MPT model parameter of the `model`.
#'   In this case, the LHS of each formula needs to give the parameters name.
#'   Furthermore, the name of the response variable needs to be passed via the
#'   `response` argument for data in long format.
#'
#'
#' @returns An object of class `mpt_formula` which is a list containing the
#'   following slots:
#' 1. `formulas`: A `list` of formulas for each MPT model parameter.
#' 2. `response`: A one-sided `formula` given the response variable on the RHS.
#' 3. `brms_formula`: The `brmsformula` object created by [brms::brmsformula()].
#' 4. `model`: The `mpt_model` object passed in the `model` argument.
#' 5. `data_format`: see the corresponding argument
#'
#' The [brms::stancode] and [brms::standata] methods for `mpt_formula` objects
#' return the same objects as the corresponding default `brms` methods (which
#' are internally called).
#'
#' @example examples/examples_mpt_formula.R
#'
#' @importFrom stats as.formula
#' @order 1
#' @export
mpt_formula <- function(formula, ..., response, model,
                        data_format = "long",
                        brms_args = list()) {
  if (missing(model)) {
    stop("model object needs to be provided.", call. = FALSE)
  }

  data_format <- match.arg(data_format, c("long", "wide", "aggregated"))
  if (data_format == "aggregated") data_format <- "wide"

  dots <- list(...)
  dots_not_formulas <- dots[!vapply(dots, FUN = inherits,
                                    FUN.VALUE = TRUE, what = "formula")]
  if (length(dots_not_formulas) > 0) {
    stop("Non-formula arguments in '...' ignored: ",
         paste(names(dots_not_formulas), collapse = ", "), call. = FALSE)
  }
  dots_formulas <- dots[vapply(dots, FUN = inherits,
                                 FUN.VALUE = TRUE, what = "formula")]

  if (! missing(response) & data_format == "wide") {
    message("response argument ignored")
  }
  if (data_format == "wide" & ! is_rhs_only(formula)) {
    message("response variable specified in formula ignored")
    formula <- make_rhs_only(formula)
  }

  # One formula for all parameters
  if (length(dots_formulas) == 0) {
    if (is_rhs_only(formula) & data_format == "long" & missing(response)) {
      stop("To specify a formula for long-format data, a response variable must be provided, either via the response argument or the LHS of the formula.")
    }

    formula_out <- vector("list", length(model$parameters)) # 1st formula with resp
    full_formula <- vector("list", length(model$parameters)) # all formulas with parameters
    for (i in seq_along(model$parameters)) {
      if (data_format == "wide") {
        # full_formula has the same structure for all forms
        full_formula[[i]] <- as.formula(paste0(as.name(model$parameters[i]),
                                               " ~ ",
                                               as.character(formula)[2]),
                                        env = globalenv())
        if (i == 1) {
          # -> first formula_out needs to have the brms vint() structure
          all_cats <- unique(unlist(lapply(attr(model$list, "cat_map"), function(x) return(x))))
          form_str <- paste0(all_cats[1],
                             " | vint(",
                             paste0(all_cats[-1], collapse = ", "),
                             ") ~ ",
                             as.character(formula, sep = "")[2])
          formula_out[[1]] <- as.formula(form_str, env = globalenv())
        } else formula_out[[i]] <- full_formula[[i]]
      } else {
        # 1st formula
        formula_out[[i]] <- formula
        # other formulas
        if (i > 1) formula_out[[i]][[2]] <- as.name(model$parameters[i])
        full_formula[[i]] <- formula
        full_formula[[i]][[2]] <- as.name(model$parameters[i])
      }
    }
    if (data_format == "long") {
      if (!missing(response)) message("response argument ignored.")
      response <- as.formula(paste("~", formula_out[[1]][[2]]),
                             env = globalenv())
    } else response <- NULL

  } else { # Individual formulas for parameters
    all_formulas <- c(formula, dots_formulas)
    if (!all(vapply(all_formulas, length, 1L) == 3)) {
      stop("all formulas need to have LHS and RHS", call. = FALSE)
    }
    if (missing(response)) {
      if (data_format == "long") stop("response cannot be missing if
                                      data_format = 'long' and individual
                                      formulas are provided.",
                      call. = FALSE)
    } else if (data_format == "wide") {
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
    if (data_format == "wide") {
      all_cats <- unique(unlist(lapply(attr(model$list, "cat_map"), function(x) return(x))))
      form_str <- paste0(all_cats[1],
                         " | vint(",
                         paste0(all_cats[-1], collapse = ", "),
                         ") ~ ",
                         as.character(formula, sep = "")[2])
      formula_out[[1]] <- as.formula(form_str, env = globalenv())
    } else formula_out[[1]][[2]] <- response[[2]]
    full_formula <- all_formulas[match(model$parameters, all_vars_formula)]
  }

  # Make brms formula without family
  brms_formula <- do.call(what = brms::brmsformula,
                         args = c(
                           formula = formula_out[[1]],
                           flist = list(formula_out[-1])))
  if (length(brms_formula$pforms) > 0) {
    attr <- attributes(brms_formula$formula)
    for (i in seq_along(brms_formula$pforms)) {
      attributes(brms_formula$pforms[[i]]) <- attr
    }
  }

  out <- list(
    formulas = full_formula,
    formula_out = formula_out,
    response = response,
    model = model,
    brms_formula = brms_formula,
    data_format = data_format
  )
  class(out) <- c("mpt_formula")
  out
}

#' @export
print.mpt_formula <- function(x, ...) {
  if (x$data_format == "wide") {
    cat("MPT formulas for wide / aggregated data:\n", sep = "")
  } else {
    cat("MPT formulas for long / non-aggregated data (response: ",
        all.vars(x$response), "):\n", sep = "")
  }
  for (i in seq_along(x[[1]])) {
    print(x[[1]][[i]])
  }
  invisible(x)
}
