
# model_list <- s2htsm_model$list
# model_names <- s2htsm_model$names
# parameters <- s2htsm_model$parameters

make_log_lik <- function(model_list, model_names, parameters) {
  function(i, prep) {
    eval.env <- new.env()
    y <- prep$data$Y[i]
    item_type <- prep$data$item_type[i]
    n_cat <- prep$data$n_cat[i]
    length_out <- prep$ndraws
    prob_length <- vapply(model_list, length, NA_integer_)
    out <- matrix(NA_real_, nrow = length_out, ncol = length(i))
    for (isel in seq_along(i)) {
      for (j in seq_along(parameters)) {
        if (j == 1) {
          assign(x = parameters[j],
                 value = brms::get_dpar(prep, "mu", i = i[isel]),
                 envir = eval.env)
        } else {
          assign(x = parameters[j],
                 value = brms::get_dpar(prep, parameters[j], i = i[isel]),
                 envir = eval.env)
        }
      }
      prob_out <- matrix(NA_real_, nrow = length_out,
                         ncol = n_cat[isel])
      for (j in seq_len(n_cat[isel])) {
        prob_out[,j] <- eval(model_list[[item_type[isel]]][[j]],
                             envir = eval.env)
      }
      out[,isel] <- extraDistr::dcat(y[isel], prob = prob_out)
    }
    out[,]
  }
}

make_posterior_predict <- function(model_list, model_names, parameters) {
  function(i, prep, ...) {
    eval.env <- new.env()
    #y <- prep$data$Y[i]
    item_type <- prep$data$item_type[i]
    n_cat <- prep$data$n_cat[i]
    length_out <- prep$ndraws
    prob_length <- vapply(model_list, length, NA_integer_)
    out <- matrix(NA_integer_, nrow = length_out, ncol = length(i))
    for (isel in seq_along(i)) {
      for (j in seq_along(parameters)) {
        if (j == 1) {
          assign(x = parameters[j],
                 value = brms::get_dpar(prep, "mu", i = i[isel]),
                 envir = eval.env)
        } else {
          assign(x = parameters[j],
                 value = brms::get_dpar(prep, parameters[j], i = i[isel]),
                 envir = eval.env)
        }
      }
      prob_out <- matrix(NA_real_, nrow = length_out,
                         ncol = n_cat[isel])
      for (j in seq_len(n_cat[isel])) {
        prob_out[,j] <- eval(model_list[[item_type[isel]]][[j]],
                             envir = eval.env)
      }
      out[,isel] <- apply(prob_out, 1, extraDistr::rcat, n = 1)
    }
    out[,]
  }
}

make_posterior_epred <- function(model_list, model_names, parameters) {
  function(prep) {
    eval.env <- new.env()
    length_out <- prep$ndraws
    cols_out <- prep$nobs
    prob_length <- vapply(model_list, length, NA_integer_)
    out <- array(NA_real_, dim = c(length_out, cols_out, max(prob_length)))
    for (isel in seq_len(cols_out)) {
      for (j in seq_along(parameters)) {
        if (j == 1) {
          assign(x = parameters[j],
                 value = brms::get_dpar(prep, "mu", i = isel),
                 envir = eval.env)
        } else {
          assign(x = parameters[j],
                 value = brms::get_dpar(prep, parameters[j], i = isel),
                 envir = eval.env)
        }
      }
      for (j in seq_len(prep$data$n_cat[isel])) {
        out[,isel,j] <- eval(model_list[[prep$data$item_type[isel]]][[j]],
                             envir = eval.env)
      }
    }
    out[,,]
  }
}
