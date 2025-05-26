make_log_lik <- function(model_list, model_names, parameters, data_format,
                         log_p) {
  if (data_format == "long") {
    llk <- function(i, prep) {
      y <- prep$data$Y[i]
      prob_out <- get_pred_prob(i, prep, parameters, model_list)
      out <- extraDistr::dcat(y, prob = prob_out)
      return(out)
    }
  } else {
    llk <- function(i, prep) {
      resps_i <- prep_resps_wide(i, prep, model_list)
      prob_out <- get_pred_prob(i, prep, parameters, model_list)
      out <- extraDistr::dmnom(resps_i, sum(resps_i), prob = prob_out)
      return(out)
    }
  }
  return(llk)
}


make_posterior_predict <- function(model_list, model_names, parameters,
                                   data_format, log_p) {
  if (data_format == "long") {
    post_pred_fun <- function(i, prep, ...) {
      # dim: ndraws, n
      prob_out <- get_pred_prob(i, prep, parameters, model_list)
      out <- apply(prob_out, 1, extraDistr::rcat, n = 1)
      return(out)
    }
  } else {
    post_pred_fun <- function(i, prep, ...) {
      resps_i <- prep_resps_wide(i, prep, model_list)
      prob_out <- get_pred_prob(i, prep, parameters, model_list)
      out <- apply(prob_out, 1, extraDistr::rmnom, n = 1,
                   size = sum(resps_i))
      rownames(out) <- names(resps_i)
      out <- lapply(seq_len(ncol(out)), function(i) out[,i])
      # TODO: names
      return(out)
    }
  }
  return(post_pred_fun)
}

make_posterior_epred <- function(model_list, model_names, parameters) {
  post_epred_fun <- function(prep) {
    length_out <- prep$ndraws
    cols_out <- prep$nobs
    prob_length <- vapply(model_list, length, NA_integer_)
    out <- array(NA_real_, dim = c(length_out, cols_out, max(prob_length)))
    for (i in 1:prep$nobs) {
      out[, i, ] <- get_pred_prob(i, prep, parameters, model_list)
    }
    return(out)
  }
  return(post_epred_fun)
}



make_simple_pred <- function(model_list, model_names, parameters) {
  function(pars) {
    eval.env <- new.env()
    if (length(pars) != length(parameters)) {
      stop("pars is not of correct length: ",
           length(pars), " != ", length(parameters), call. = FALSE)
    }
    for (j in seq_along(parameters)) {
      assign(x = parameters[j],
             value = pars[j],
             envir = eval.env)
    }
    out <- vector("list", length(model_list))

    for (i in seq_along(out)) {
      out[[i]] <- vapply(model_list[[i]], eval, envir = eval.env, FUN.VALUE = 0)
    }
    out
  }
}


get_pred_prob <- function(i, prep, parameters, model_list) {
  eval.env <- new.env()
  n_cat <- prep$data$n_cat[i]
  item_type <- prep$data$item_type[i]
  length_out <- prep$ndraws
  for (j in seq_along(parameters)) {
    if (j == 1) {
      assign(x = parameters[j],
             value = brms::get_dpar(prep, "mu", i = i),
             envir = eval.env)
    } else {
      assign(x = parameters[j],
             value = brms::get_dpar(prep, parameters[j], i = i),
             envir = eval.env)
    }
  }
  prob_out <- matrix(NA_real_, nrow = length_out,
                     ncol = n_cat)
  for (j in seq_len(n_cat)) {
    prob_out[,j] <- eval(model_list[[item_type]][[j]],
                         envir = eval.env)
  }
  return(prob_out)
}


prep_resps_wide <- function(i, prep, model_list) {
  item_type <- prep$data$item_type[i]
  cat_names <- unique(unlist(lapply(attr(model_list, "cat_map")[item_type],
                                    function(x) paste0("cat_", x))))
  cat_names[1] <- "Y"
  resps_i <- sapply(prep$data[cat_names], function(x) x[i])
  return(resps_i)
}

