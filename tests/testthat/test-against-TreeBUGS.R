test_that("ppp_test() gives similar estimates and X2 and G2 statistics as TreeBUGS", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("TreeBUGS")
  library("dplyr")
  library("tidyr")
  library("TreeBUGS")

  skk_red <- subset(skk13, (race == "german"))
  options(mc.cores = 2)
  EQNFILE <- system.file("extdata", "u2htm.eqn", package = "mptstan")
  suppressWarnings(u2htsm_model <- make_mpt(EQNFILE, restrictions = list("Do = Dn")))
  u2htm_formula <- mpt_formula(resp ~ 1 + (1 | id),
                               model = u2htsm_model)
  fit_skk <- mpt(resp ~ 1 + (1 |s| id), model = u2htsm_model, data = skk_red,
                 tree = "type",
                 init_r = 0.5,
                 chains = 2,
                 refresh = 0, open_progress = FALSE
  )
  skkagg <- skk13 |>
    dplyr::filter(race == "german") |>
    dplyr::count(race, resp, id, type) |>
    tidyr::pivot_wider(names_from = resp, values_from = n, values_fill = 0)

  # Fit equivalent model with aggregated data
  u2htm_formula_agg <- mpt_formula(~ 1 + (1 |s| id), model = u2htsm_model, data_format = "wide")
  fit_skk_agg <- mpt(u2htm_formula_agg, data = skkagg,
                     tree = "type",
                     init_r = 0.5,
                     chains = 2,
                     refresh = 0, open_progress = FALSE
  )

  # Fit equivalent TreeBUGS model
  model <- "8
old old_old Do
old old_old (1-Do)*(1-g1)*g2
old old_unsure (1-Do)*g1
old old_new (1-Do)*(1-g1)*(1-g2)
new new_old (1-Dn)*(1-g1)*g2
new new_unsure (1-Dn)*g1
new new_new Dn
new new_new (1-Dn)*(1-g1)*(1-g2)"

  skk_tb <- skk_red |>
    dplyr::count(id, race, type, resp, .drop = FALSE) |>
    tidyr::pivot_wider(names_from = c(type, resp), values_from = n)
  skk_tb <- skk_tb |>
    filter(race == "german")

  fit_tb <- suppressWarnings(traitMPT(model, skk_tb,
                                      restrictions = list("Do = Dn")))

  ####### Compare parameter estimates
  expect_equal(fixef(fit_skk)[, 1],
               fit_tb$summary$groupParameters$mu[, 1],
               tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(fixef(fit_skk)[, 3],
               fit_tb$summary$groupParameters$mu[, 3],
               tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(fixef(fit_skk_agg)[, 1],
               fit_tb$summary$groupParameters$mu[, 1],
               tolerance = 1e-1, ignore_attr = TRUE)
  expect_equal(fixef(fit_skk_agg)[, 3],
               fit_tb$summary$groupParameters$mu[, 3],
               tolerance = 1e-1, ignore_attr = TRUE)

  # Compute ppp_test values for both
  t_mptstan <- ppp_test(fit_skk, type = "X2")
  t_mptstan_agg <- ppp_test(fit_skk_agg, type = "X2")
  t_tb <- PPP(fit_tb, M = 10000, type = "X2")

  t_mptstan_g2 <- ppp_test(fit_skk, type = "G2")
  t_mptstan_agg_g2 <- ppp_test(fit_skk_agg, type = "G2")
  t_tb_g2 <- PPP(fit_tb, M = 10000, type = "G2")

  # Compare real frequencies from data and T1 function
  expect_equal(as.numeric(t_mptstan$n_real), as.numeric(colSums(skk_tb[3:8])))
  expect_equal(as.numeric(t_mptstan_agg$n_real), as.numeric(colSums(skk_tb[3:8])))
  expect_equal(as.numeric(sort(t_mptstan$n_real)), as.numeric(sort(colSums(t_tb$freq.obs))))
  expect_equal(as.numeric(t_mptstan_g2$n_real), as.numeric(colSums(skk_tb[3:8])))
  expect_equal(as.numeric(sort(t_mptstan_g2$n_real)), as.numeric(sort(colSums(t_tb_g2$freq.obs))))

  # Compare T1 statistics of aggregated and non-aggregated
  expect_equal(t_mptstan$t1_data, t_mptstan_agg$t1_data, tolerance = 1e-1)
  expect_equal(t_mptstan$t1_model, t_mptstan_agg$t1_model, tolerance = 1e-1)

  # Compare p values (with tolerance)
  expect_equal(t_tb$T1.p, t_mptstan$t1_p.value, tolerance = 1e-1)
  expect_equal(t_mptstan_agg$t1_p.value, t_mptstan$t1_p.value,
               tolerance = 1e-1)
  expect_equal(t_tb_g2$T1.p, t_mptstan_g2$t1_p.value, tolerance = 1e-1)
  expect_equal(t_tb_g2$T1.p, t_mptstan_g2$t1_p.value, tolerance = 1e-1)
  expect_equal(t_mptstan_agg_g2$t1_p.value, t_mptstan_g2$t1_p.value,
               tolerance = 1e-1)

  # compare p values for different numbers of draws
  t_500 <- ppp_test(fit_skk, ndraws = 500)
  t_1000 <- ppp_test(fit_skk, ndraws = 1000)
  t_2000 <- ppp_test(fit_skk, ndraws = 2000)

  t_500_agg <- ppp_test(fit_skk_agg, ndraws = 500)
  t_1000_agg <- ppp_test(fit_skk_agg, ndraws = 1000)
  t_2000_agg <- ppp_test(fit_skk_agg, ndraws = 2000)


  expect_equal(t_500$t1_model, t_1000$t1_model, tolerance = 1e-1)
  expect_equal(t_500$t1_model, t_2000$t1_model, tolerance = 1e-1)
  expect_equal(t_500$t1_model, t_500_agg$t1_model, tolerance = 1e-1)
  expect_equal(t_500$t1_model, t_1000_agg$t1_model, tolerance = 1e-1)
  expect_equal(t_500$t1_model, t_2000_agg$t1_model, tolerance = 1e-1)

})
