
#' @export
mpt <- function(formula, model, data, tree, ...) {
  if (inherits(formula, "formula")) {
    formula <- mpt_formula(formula = formula, model = model)
  } else if (!inherits(formula, "mpt_formula")) {
    stop("formula needs to be a formula or mpt_formula object.",
         call. = FALSE)
  }
  call <- match.call()
  data_prep <- prep_data(formula = formula, data = data, tree = tree)
  stanvars <- prep_stanvars(formula = formula, data_prep = data_prep)
  #standata <- standata(object = formula, data = data, tree = tree)
  #str(standata, 1)
  #browser()

  #stancode(object = formula, data = data, tree = tree)

  out <- do.call(brms::brm, args = c(
    formula = list(formula$brmsformula),
    data = list(data_prep),
    family = list(formula$model$family),
    stanvars = list(stanvars),
    list(...)
  ))
  out$call <- call
  out$mpt_formula <- formula
  out$data$mpt_tree <- data_prep[[tree]]
  out$data$mpt_n_categories <- data_prep$mpt_n_categories
  out$data$mpt_response <- data_prep$mpt_original_response
  out$orig_data <- data
  out$data.name <- call[["data"]]
  class(out) <- c("mpt_fit", class(out))
  return(out)
}

