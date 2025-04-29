#' Posterior Predictive P-values for MPTs
#'
#' Calculates posterior predictive p-values for MPT values. Currently only the
#' T1 statistic of Klauer (2010) is implemented.
#'
#' @param object fitted MPT model object returned by [mpt()].
#' @param ndraws number of posterior draws that should be used for calculating
#'   the posterior predictive p-values.
#'
#' @references Klauer, K. C. (2010). Hierarchical Multinomial Processing Tree
#'   Models: A Latent-Trait Approach. *Psychometrika*, 75(1), 70-98.
#'   https://doi.org/10.1007/s11336-009-9141-0
#'
#' @export
ppp_test <- function(object, ndraws = 500) {
  if (!inherits(object, "mpt_fit")) {
    stop("object needs to be of class 'mpt_fit'.", call. = FALSE)
  }

  pepred <- brms::posterior_epred(object, ndraws = ndraws)
  dims <- dim(pepred)
  ndraws <- dims[1]
  #pepred[1,1:6,]

  resp_var <- object$data[[attr(attr(object$data, "terms"), "term.labels")[1]]]
  # resp_mat <- pepred[1,,]
  # resp_mat[!is.na(resp_mat)] <- 0
  # resp_mat[cbind(seq_len(dims[2]), resp_var)] <- 1

  test_dat <- apply(pepred, 1,
                    function(x) extraDistr::dcat(resp_var, prob = x, log = TRUE))
  test_dat_sum <- apply(test_dat, 2, sum)

  # t1_dat <- apply(pepred, 1, function(x) sum((resp_mat - x)^2  ))
  # head(resp_mat)


  #t1_dat <- apply(pepred, 1, function(x) sum(1/(1 - x[resp_mat == 1])^2  ))
  #str(t1_dat)


  # sim_mats <- apply(pepred, 1,
  #                   function(x) extraDistr::rmnom(rep(1, dims[2]), 1, x),
  #                   simplify = FALSE)

  sim_vec <- apply(pepred, c(1, 2), function(x) extraDistr::rcat(1, prob = x))
  #str(sim_vec)

  test_model <- vector("numeric", ndraws)
  for (i in seq_len(ndraws)) {
    test_model[i] <- sum(extraDistr::dcat(sim_vec[i,], prob = pepred[i,,], log = TRUE))
    #t1_model[i] <- sum((sim_mats[[i]] - pepred[i,,])^2)
    #t1_model[i] <- sum(1/(1 - pepred[i,,][sim_mats[[i]] == 1])^2)
  }
  out <- c("p.value" = mean(test_model > test_dat_sum) ,
           "ll_model" = log_mean_exp(test_model),
           "ll_data" = log_mean_exp(test_dat_sum)
  )
  return(out)
}

log_mean_exp <- function(x) {
  max_x <- max(x)
  max_x + log(sum(exp(x - max_x))) - log(length(x))
}
