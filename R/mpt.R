
#' @export
mpt <- function(formula, model, data, tree, ...) {
  if (inherits(formula, "formula")) {
    formula <- mptformula(formula = formula, model = model)
  } else if (!inherits(formula, "mpt_formula")) {
    stop("formula needs to be a formula or mpt_formula object.",
         call. = FALSE)
  }
  data_prep <- prep_data(formula = formula, data = data, tree = tree)
  stanvars <- prep_stanvars(formula = formula, data_prep = data_prep)
  do.call(brms::brm, args = c(
    formula = list(formula$brmsformula),
    data = list(data_prep),
    family = list(formula$model$family),
    stanvars = list(stanvars),
    list(...)
  ))
}

