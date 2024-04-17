#' Fit Bayesian MPT Models with brms & Stan
#'
#' @param formula Either a `formula` that is applied to each MPT model parameter
#'   or a `mpt_formula` object (created by [mpt_formula()]) specifying separate
#'   formulas for each parameter. If a `formula`, the LHS needs to specify the
#'   response variable.
#' @param data `data.frame` containing the variables in `formula`. Data needs to
#'   be on an observation-level (i.e., each row is one response/observation) and
#'   cannot be aggregated in any way.
#' @param tree one-sided formula or character specifying the variable in `data`
#'   indicating the tree (or item type) of a given observation. The values of
#'   the `tree` variable need to match the names of the trees in `model`. Can be
#'   omitted for models with only one tree.
#' @param model `mpt_model` object as created by [make_mpt()], ignored if
#'   `formula` is an object of `mpt_formula`.
#' @param ... Further arguments passed to [brms::brm()] such as `chains`,
#'   `iter`, `warmup`, and `cores`.
#'
#' @returns A fitted model object returned from [brms::brm] of class `brmsfit`
#'   with additional class `mpt_fit` and some extra slots (i.e., `call`,
#'   `mpt_formula`, and `orig_data`). This object should seemlessly interact
#'   with the `brms` ecosystem (e.g., can be used for obtaining posterior
#'   predictive values and information criteria, see `Examples`).
#'
#' @example examples/examples_mpt.R
#' @export
mpt <- function(formula, data, tree, model, ...) {
  if (inherits(formula, "formula")) {
    formula <- mpt_formula(formula = formula, model = model)
  } else if (inherits(formula, "mpt_formula")) {
    if (!missing(model)) {
      message("model argument replaced with model object from mpt_formula.")
    }
    model <- formula$model
  } else if (!inherits(formula, "mpt_formula")) {
    stop("formula needs to be a formula or mpt_formula object.",
         call. = FALSE)
  }
  if (missing(tree)) {
    if (model$ns["trees"] == 1) {
      data_tree[["mpt_tree"]] <- s2htsm_model$names$trees
      tree <- "mpt_tree"
    } else {
      stop("tree cannot be missing for models with more than 1 tree.",
           call. = FALSE)
    }
  }
  call <- match.call()
  data_prep <- prep_data(formula = formula, data = data, tree = tree)
  stanvars <- prep_stanvars(formula = formula, data_prep = data_prep)
  out <- do.call(brms::brm, args = c(
    formula = list(formula$brmsformula),
    data = list(data_prep),
    family = list(formula$model$family),
    stanvars = list(stanvars),
    list(...)
  ))
  out$call <- call
  out$mpt_formula <- formula
  out$data$mpt_tree <- factor(data_prep[[tree]], levels = model$names$trees)
  out$data$mpt_n_categories <- data_prep$mpt_n_categories
  out$data$mpt_response <- data_prep$mpt_original_response
  out$orig_data <- data
  out$data.name <- call[["data"]]
  class(out) <- c("mpt_fit", class(out))
  return(out)
}

