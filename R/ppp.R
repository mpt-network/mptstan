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
ppp <- function(object, ndraws, type = "X2") {
  if (!inherits(object, "mpt_fit")) {
    stop("object needs to be of class 'mpt_fit'.", call. = FALSE)
  }

  if (missing(ndraws)) ndraws <- brms::niterations(object)

  pepred <- brms::posterior_epred(object, ndraws = ndraws)

  # Generate the correct data structure for the contingency table:
  # one column for each response category x tree combination
  trees_names <- unique(object$data$mpt_tree)
  unique_tree_cats <- length(unlist(attr(object$mpt_formula$model$list, "cat_map")))
  pepred_agg <- matrix(NA, nrow = ndraws, ncol = unique_tree_cats)
  for (i in seq_along(trees_names)) {
    tn_tmp <- trees_names[i]
    n_resp_tmp <- length(attr(object$mpt_formula$model$list, "cat_map")[[tn_tmp]])
    pepred_agg[, ((i - 1) * n_resp_tmp + 1):(i * n_resp_tmp)] <-
      apply(pepred[, object$data$mpt_tree == tn_tmp, ], MARGIN = c(1, 3),
            FUN = sum)
  }
  # Format for expected, real and simulated data, r response categories and
  # t trees should be:
  # tree-1-resp-1, ..., tree-1-resp-r, ... tree-c-resp-1, ..., tree-c-resp-r
  if (object$mpt_formula$data_format == "long") {
    object$data$count_var <- 1:nrow(object$data)
    dat_agg <- stats::aggregate(count_var ~ resp * mpt_tree, data = object$data,
                         FUN = length)$count_var
  } else {
    cats <- unique(unlist(lapply(attr(object$mpt_formula$model$list, "cat_map"),
                                 function(x) x)))
    dat_agg <- data.frame(aggregate(. ~ mpt_tree, data = object$data,
                                    FUN = sum))[, cats]
    dat_agg <- as.vector(t(dat_agg))
  }

  t1_dat <- apply(pepred_agg, MARGIN = 1, FUN = function(x) {
    compute_T1(x, dat_agg, type)
  })

  # simulate data based on the _same_ posterior draws

  if (object$mpt_formula$data_format == "long") {
    sim_dat <- array(NA, dim = c(dim(pepred)[1], dim(pepred)[2]))
    for (i in 1:ndraws) {
      sim_dat[i, ] <- extraDistr::rcat(n = dim(pepred)[2],
                                          prob = pepred[i, , ])
    }
    sim_dat_agg <- t(apply(sim_dat, MARGIN = 1, FUN = function(x) {
      matrix(table(x, object$data$mpt_tree), ncol = unique_tree_cats)
    }))
  } else {
    sim_dat <- array(NA, dim = dim(pepred))
    n_obs <- dim(pepred)[2]
    for (i in 1:n_obs) {
      sim_dat[, i, ] <- extraDistr::rmnom(n = dim(pepred)[1],
                                          size = sum(object$data[1, cats],
                                                     na.rm = T),
                                          prob = pepred[, i, ])
    }
    sim_dat_agg <- t(apply(sim_dat, MARGIN = 1, FUN = function(x) {
      row_tmp <- data.frame(x)
      names(row_tmp) <- cats
      row_tmp$mpt_tree <- object$data$mpt_tree

      row_tmp <- data.frame(aggregate(. ~ mpt_tree, data = row_tmp,
                                      FUN = sum))[, cats]
      row_tmp <- as.vector(t(row_tmp))
      return(row_tmp)
    }))
  }

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
print.post_pred_p <- function(x, ...) {
  cat(
    " ## Mean structure (T1):\n",
    "Observed = ", mean(x$t1_data),
    "; Predicted = ", mean(x$t1_model),
    "; p-value = ", mean(x$t1_p.value), "\n"
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

