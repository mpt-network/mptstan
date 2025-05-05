

test_that("ppp() gives similar X2 and G2 statistics as TreeBUGS", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("TreeBUGS")
  library("dplyr")
  library("tidyr")
  library("TreeBUGS")

  options(buildtools.check = function(action) FALSE )

  skk_red <- subset(skk13, (race == "german"))
  # Fit mptstan() model
  options(mc.cores = parallel::detectCores())
  EQNFILE <- system.file("extdata", "u2htm.eqn", package = "mptstan")
  suppressWarnings(u2htsm_model <- make_mpt(EQNFILE, restrictions = list("Do = Dn")))
  u2htm_formula <- mpt_formula(resp ~ 1 + (1 | id),
                               model = u2htsm_model)
  fit_skk <- mpt(u2htm_formula, data = skk_red,
                 tree = "type",
                 init_r = 0.5
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
    count(id, race, type, resp, .drop = FALSE) |>
    pivot_wider(names_from = c(type, resp), values_from = n)
  skk_tb <- skk_tb |>
    filter(race == "german")

  fit_tb <- traitMPT(model, skk_tb, restrictions = list("Do = Dn"))

  # Compute ppp values for both
  t_mptstan <- ppp_test(fit_skk, ndraws = 4000, type = "X2")
  t_tb <- PPP(fit_tb, M = 10000, type = "X2")

  t_mptstan_g2 <- ppp_test(fit_skk, ndraws = 4000, type = "G2")
  t_tb_g2 <- PPP(fit_tb, M = 10000, type = "G2")

  # Compare real frequencies from data and T1 function
  expect_equal(as.numeric(t_mptstan$n_real), as.numeric(colSums(skk_tb[3:8])))
  expect_equal(as.numeric(sort(t_mptstan$n_real)), as.numeric(sort(colSums(t_tb$freq.obs))))

  # Compare p values (with tolerance)
  expect_equal(t_tb$T1.p, t_mptstan$t1_p.value, tolerance = 1e-1)

})
