

prep_data <- function(formula, data, tree) {
  data_prep <- as.data.frame(data) ## needed for working with tibbles
  if (missing(tree)) {
    if (formula$model$ns["trees"] == 1) {
      data_prep[["mpt_tree"]] <- formula$model$names$trees
      tree <- "mpt_tree"
    } else {
      stop("tree cannot be missing for models with more than 1 tree.",
           call. = FALSE)
    }
  }
  resp_char <-  parse_single_var(formula$response, data_prep, "response")
  tree_char <-  parse_single_var(tree, data_prep, "response")
  data_prep[["mpt_original_response"]] <- data_prep[[resp_char]]
  data_prep[[resp_char]] <- NA_integer_
  data_prep[["mpt_n_categories"]] <- NA_integer_
  data_prep[["mpt_item_type"]] <- NA_integer_
  ntrees <- length(formula$model$names$trees)
  for (i in seq_len(ntrees)) {
    data_prep[ data_prep[[tree_char]] == formula$model$names$trees[i],
               resp_char ] <- as.numeric(
                 factor(x = data_prep[ data_prep[[tree_char]] ==
                                         formula$model$names$trees[i],
                                       "mpt_original_response" ],
                        levels = formula$model$names$categories[[i]]
                 )
               )
    data_prep[ data_prep[[tree_char]] == formula$model$names$trees[i],
               "mpt_n_categories"] <- length(formula$model$names$categories[[i]])
    data_prep[ data_prep[[tree_char]] == formula$model$names$trees[i],
               "mpt_item_type"] <- i
  }
  return(data_prep)
}

prep_stanvars <- function(formula, data_prep) {
  brms::stanvar(scode = formula$model$brms_llk,
                block = "functions") +
    brms::stanvar(data_prep$mpt_item_type, name = "item_type",
                  scode = "  int item_type[N];") +
    brms::stanvar(data_prep$mpt_n_categories, name = "n_cat",
                  scode = "  int n_cat[N];")
}

get_default_priors <- function(formula, data, prior_intercept, prior_coef) {
  dp <- default_prior(formula$brmsformula, data = data)
  default_prior <- brms::empty_prior()
  class_intercept <- dp$class == "Intercept"
  if (sum(class_intercept) > 0) {
    default_prior <- default_prior +
      brms::set_prior(prior_intercept, class = "Intercept",
                      dpar = unique(dp$dpar[class_intercept]))
  }
  b_coef <- (dp$class == "b") & (dp$coef != "Intercept") & (dp$coef != "")
  if (sum(b_coef) > 0) {
    default_prior <- default_prior +
      brms::set_prior(prior_coef, class = "b",
                      dpar = unique(dp$dpar[b_coef]))
  }
  b_intercept <- (dp$class == "b") & (dp$coef == "Intercept")
  if (sum(b_intercept) > 0) {
    default_prior <- default_prior +
      brms::set_prior(prior_intercept, class = "b",
                dpar = unique(dp$dpar[b_intercept]))
  }
  default_prior
}

#' @importFrom brms stancode
#' @rdname mpt_formula
#' @param object An object of class `mpt_formula`
#' @order 2
#' @export
stancode.mpt_formula <- function(object, data,
                                 default_prior_intercept = "normal(0, 1)",
                                 default_prior_coef = "normal(0, 0.5)",
                                 default_priors = TRUE,
                                 tree,
                                 ...) {
  data_prep <- prep_data(formula = object, data = data, tree = tree)
  stanvars <- prep_stanvars(object, data_prep)
  dots <- list(...)
  if (default_priors) {
    dp <- get_default_priors(formula = object, data = data_prep,
                             prior_intercept = default_prior_intercept,
                             prior_coef = default_prior_coef)
    if ("prior" %in% names(dots)) {
      dots$prior <- dots$prior + dp
    } else {
      dots$prior <- dp
    }
  }
  do.call(brms::stancode,
          args = c(
            object = list(object$brmsformula), data = list(data_prep),
            family = list(object$model$family),
            stanvars = list(stanvars),
            dots
          ))
}

#' @importFrom brms standata
#' @rdname mpt_formula
#' @order 2
#' @inheritParams mpt
#' @export
standata.mpt_formula <- function(object, data,
                                 tree,
                                 ...) {
  data_prep <- prep_data(formula = object, data = data, tree = tree)
  out <- do.call(brms::standata,
                 args = c(
                   object = list(object$brmsformula),
                   data = list(data_prep),
                   family = list(object$model$family),
                   list(...)
                 ))
  return(out)
}


parse_single_var <- function(x, data, argument) {
  if (inherits(x, "formula")) {
    out <- all.vars(x)
  } else if (is.character(x)) {
    out <- x
  } else {
    stop(argument, " needs to be either a character or on-sided formula",
         call. = FALSE)
  }
  if (length(out) != 1) {
    stop(argument, " needs to contain one variable only",
         call. = FALSE)
  }
  if (!(out %in% colnames(data))) {
    stop(out, " is not a variable in data", call. = FALSE)
  }
  out
}


