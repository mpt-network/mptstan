

prep_data <- function(formula, data, tree) {
  data_prep <- data
  if (missing(tree)) {
    stop("tree variable needs to be provided.", call. = FALSE)
  }
  resp_char <-  parse_single_var(formula$response, data, "response")
  tree_char <-  parse_single_var(tree, data, "response")
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

#' @importFrom brms stancode
#' @export
stancode.mpt_formula <- function(object, data,
                                 tree,
                                 ...) {
  data_prep <- prep_data(formula = object, data = data, tree = tree)
  stanvars <- prep_stanvars(object, data_prep)
  do.call(brms::stancode,
          args = c(
            object = list(object$brmsformula), data = list(data_prep),
            family = list(object$model$family),
            stanvars = list(stanvars),
            list(...)
          ))
}

#' @importFrom brms standata
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


