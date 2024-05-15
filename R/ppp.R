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

  pepred <- posterior_epred(object, ndraws = ndraws)
  dims <- dim(pepred)

  resp_var <- object$data[[attr(attr(object$data, "terms"), "term.labels")[1]]]
  resp_mat <- pepred[1,,]
  resp_mat[!is.na(resp_mat)] <- 0
  resp_mat[cbind(seq_len(dims[2]), resp_var)] <- 1

  t1_dat <- apply(pepred, 1, function(x) sum((resp_mat - x)^2 ))

  sim_mats <- apply(pepred, 1,
                    function(x) extraDistr::rmnom(rep(1, dims[2]), 1, x),
                    simplify = FALSE)
  t1_model <- vector("numeric", ndraws)
  for (i in seq_len(ndraws)) {
    t1_model[i] <- sum((sim_mats[[i]] - pepred[i,,])^2 )
  }
  out <- c("t1_p.value" = mean(t1_model > t1_dat),
           "t1_model" = mean(t1_model),
           "t1_data" = mean(t1_dat)
  )
  return(out)
}
