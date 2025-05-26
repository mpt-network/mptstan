#' Fit Bayesian MPT Models with brms & Stan
#'
#' Fit a Bayesian multinomial processing tree (MPT) model using [brms::brm()]
#' (so ultimately `Stan`) to trial-level data, potentially including a
#' multilevel/hierarchical model structure. To fit an MPT, first create a model
#' object using [make_mpt()] and then pass the model object, the data, as well
#' as a `formula` providing a symbolic description of the regression (i.e.,
#' fixed-effect) and hierarchical (i.e., random-effect) structure. The returned
#' object can be used for model post-processing.
#'
#' @param formula Either a `formula` that is applied to each MPT model parameter
#'   or a `mpt_formula` object (created by [mpt_formula()]) specifying separate
#'   formulas for each parameter. If a `formula`, the LHS needs to specify the
#'   response variable.
#' @param data `data.frame` containing the variables in `formula`. Data needs to
#'   be on an observation-level (i.e., each row is one response/observation) and
#'   cannot be aggregated in any way.
#'   TODO: change this
#' @param tree one-sided formula or character specifying the variable in `data`
#'   indicating the tree (or item type) of a given observation. The values of
#'   the `tree` variable need to match the names of the trees in `model`. Can be
#'   omitted for models with only one tree.
#' @param model `mpt_model` object as created by [make_mpt()], ignored if
#'   `formula` is an object of `mpt_formula`.
#' @param default_prior_intercept character string describing the prior applied
#'   to the fixed-effect intercepts for each MPT model parameter on the
#'   unconstrained scale (if `default_priors = TRUE`). The default, `"normal(0,
#'   1)"` implies a flat prior on the MPT parameter scale.
#' @param default_prior_coef character string describing the prior applied to
#'   the non-intercept fixed-effect parameters for each MPT model parameter on
#'   the unconstrained scale (if `default_priors = TRUE`).
#' @param default_priors logical value indicating whether (the default, `TRUE`)
#'   or not (`FALSE`) the priors specified via the `default_prior_intercept` and
#'   `default_prior_coef` argument should be applied.
#' @param data_format character string indicating whether the formula is to be
#'   generated for fitting data in long format / non-aggregated data (`long`,
#'   the default), where a single variable contains trial-level responses, or
#'   for data in wide format / aggregated data (`wide`), where a separate column
#'   for each response category contains the respective frequency. Only used if
#'   a formula object (not an mpt_formula) is given as input and ignored
#'   otherwise.
#' @param log_p logical value indicating whether the likelihood should be
#'   evaluated with probabilities (the default, `FALSE`) or log
#'   probabilities (`TRUE`). Only used if a formula object (not an mpt_formula)
#'   is given as input and ignored otherwise. Setting `log_p` to `TRUE` can help
#'   in case of convergence issues but might be slower.
#' @param link character specifying the link function for transforming from
#'   unconstrained space to MPT model parameter (i.e., 0 to 1) space. Default is
#'   `"probit"`. Only used if a formula object (not an mpt_formula)
#'   is given as input and ignored otherwise.
#' @param ... Further arguments passed to [brms::brm()] such as `prior`,
#'   `chains`, `iter`, `warmup`, and `cores`.
#'
#' @returns A fitted model object returned from [brms::brm()] of class `brmsfit`
#'   with additional class `mpt_fit` and some extra slots (i.e., `call`,
#'   `mpt_formula`, and `orig_data`). This object should seemlessly interact
#'   with the `brms` ecosystem (e.g., can be used for obtaining posterior
#'   predictive values and information criteria, see `Examples`).
#'
#' @example examples/examples_mpt.R
#' @export
mpt <- function(formula, data, tree, model,
                default_prior_intercept = "normal(0, 1)",
                default_prior_coef = "normal(0, 0.5)",
                default_priors = TRUE,
                data_format = "long",
                log_p = F,
                link = "probit",
                ...) {
  if (inherits(formula, "formula")) {
    # add stuff for brms_family here
    # aggregated data
    formula <- mpt_formula(formula = formula, model = model,
                           data_format = data_format, log_p = log_p,
                           link = link)
  } else if (inherits(formula, "mpt_formula")) {
    if (!missing(model)) {
      message("model argument replaced with model object from mpt_formula.")
    }
    model <- formula$model
    if (!missing(log_p)) {
      message("Ignoring log_p argument and using the given mpt_formula.")
    }
    if (!missing(data_format)) {
      message("Ignoring data_format argument and using the given mpt_formula.")
    }
    if (!missing(link)) {
      message("Ignoring link argument and using the given mpt_formula.")
    }
  } else if (!inherits(formula, "mpt_formula")) {
    stop("formula needs to be a formula or mpt_formula object.",
         call. = FALSE)
  }
  # @Henrik: Why is this doubled?
  if (missing(tree)) {
    if (model$ns["trees"] == 1) {
      data[["mpt_tree"]] <- model$names$trees
      tree <- "mpt_tree"
    } else {
      stop("tree cannot be missing for models with more than 1 tree.",
           call. = FALSE)
    }
  }
  call <- match.call()
  dots <- list(...)
  data_prep <- prep_data(formula = formula, data = data, tree = tree)
  stanvars <- prep_stanvars(formula = formula, data_prep = data_prep)
  if (default_priors) {
    dp <- get_default_priors(formula = formula, data = data_prep,
                             prior_intercept = default_prior_intercept,
                             prior_coef = default_prior_coef)
    if ("prior" %in% names(dots)) {
      dots$prior <- dots$prior + dp
    } else {
      dots$prior <- dp
    }
  }
  out <- do.call(brms::brm, args = c(
    formula = list(formula$brmsformula),
    data = list(data_prep),
    family = list(formula$model$family),
    stanvars = list(stanvars),
    dots
  ))

  out$call <- call
  out$mpt_formula <- formula
  out$data$mpt_tree <- factor(data_prep[[tree]], levels = model$names$trees)
  out$data$mpt_n_categories <- data_prep$mpt_n_categories
  if (formula$data_format == "long") {
    out$data$mpt_response <- data_prep$mpt_original_response
  }
  out$orig_data <- data
  out$data.name <- call[["data"]]
  class(out) <- c("mpt_fit", class(out))
  return(out)
}

