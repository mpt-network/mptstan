#' Posterior Predictive P-values for MPTs
#'
#' Calculates posterior predictive p-values for MPT values. Currently only the
#' T1 statistic of Klauer (2010) is implemented.
#'
#' @param object fitted MPT model object returned by [mpt()].
#' @param ndraws number of posterior draws that should be used for calculating
#'   the posterior predictive p-values.
#' @param type whether the T1 statistic of expected frequencies is computed using Person's "X2"(default) or the likelihood-ratio statistic "G2"
#'
#' @references Klauer, K. C. (2010). Hierarchical Multinomial Processing Tree
#'   Models: A Latent-Trait Approach. *Psychometrika*, 75(1), 70-98.
#'   https://doi.org/10.1007/s11336-009-9141-0
#'
#' @export
ppp_test <- function(object, ndraws = 500, type = "X2") {
  if (!inherits(object, "mpt_fit")) {
    stop("object needs to be of class 'mpt_fit'.", call. = FALSE)
  }

  pepred <- brms::posterior_epred(object, ndraws = ndraws)

  # Generate the correct data structure for the contingency table:
  # one column for each response category x tree combination
  trees_names <- unique(object$data$mpt_tree)
  n_resp <- dim(pepred)[3]
  pepred_agg <- matrix(NA, nrow = ndraws, ncol = length(trees_names) * n_resp)
  for (i in seq_along(trees_names)) {
    tn_tmp <- trees_names[i]
    pepred_agg[, ((i - 1) * n_resp + 1):(i * n_resp)] <- apply(pepred[, object$data$mpt_tree == tn_tmp, ], MARGIN = c(1, 3), FUN = sum)
  }

  # Format for expected, real and simulated data, r response categories and
  # t trees should be:
  # tree-1-resp-1, ..., tree-1-resp-r, ... tree-c-resp-1, ..., tree-c-resp-r
  dat_agg <- t(matrix(table(object$data$resp, object$data$mpt_tree)))

  t1_dat <- apply(pepred_agg, MARGIN = 1, FUN = function(x) {
    compute_T1(x, dat_agg, type)
  })

  sim_dat <- brms::posterior_predict(object, ndraws = ndraws)

  # apply aggregates over posterior samples
  sim_dat_agg <- t(apply(sim_dat, MARGIN = 1, FUN = function(x) {
    matrix(table(x, object$data$mpt_tree), ncol = length(trees_names) * n_resp)
  }))

  t1_model <- vector("numeric", ndraws)
  for (i in seq_len(ndraws)) {
    t1_model[i] <- compute_T1(pepred_agg[i, ], sim_dat_agg[i, ], type)
  }

  out <- list("t1_p.value" = mean(t1_model > t1_dat),
              "t1_model" = mean(t1_model),
              "t1_data" = mean(t1_dat),
              "t1_model_all" = t1_model,
              "t1_data_all" = t1_dat,
              "n_real" = dat_agg,
              "n_exp" = pepred_agg,
              "n_pred" = sim_dat_agg)
  class(out) <- "post_pred_p"
  return(out)
}


# Taken from TreeBUGS
#' @export
print.post_pred_p <- function(object, ...) {
  cat(
    " ## Mean structure (T1):\n",
    "Observed = ", mean(object$t1_data),
    "; Predicted = ", mean(object$t1_model),
    "; p-value = ", mean(object$t1_p.value), "\n"
  )
}



# Taken from TreeBUGS
compute_T1 <- function(n_exp, n_real, type = "X2", const = 1e-8) {
  n_exp[n_exp == 0] <- const
  if (type == "X2") {
    dev <- (n_real - n_exp)^2 / n_exp
  } else if (type == "G2") {
    dev <- 2 * n_real * log(n_real / n_exp)
    dev[n_real == 0] <- 0
  }
  return(sum(dev))
}

