test_that("compare results to TreeBUGS", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("TreeBUGS")
  library("dplyr")
  library("tidyr")
  library("TreeBUGS")

  #str(skk13)
  skk_red <- subset(skk13, race == "german")
  EQNFILE <- system.file("extdata", "u2htm.eqn", package = "mptstan")
  suppressWarnings(u2htsm_model <- make_mpt(EQNFILE))
  # m1 <- mpt(resp ~ 1 + (1|s|id), skk_red, model = u2htsm_model,
  #           tree = "type", silent = 2, refresh = 0, open_progress = FALSE)
  #
  # ppp1 <- ppp_test(m1)
  # expect_equal(unname(ppp1[1]), 0.48, tolerance = 0.05)

  skkagg <- skk13 |>
    count(id, race, type, resp, .drop = FALSE) |>
    pivot_wider(names_from = c(type, resp), values_from = n)
  skkagg_g <- skkagg |>
  filter(race == "german")
  model <- "8
old old_old Do
old old_old (1-Do)*(1-g1)*g2
old old_unsure (1-Do)*g1
old old_new (1-Do)*(1-g1)*(1-g2)
new new_old (1-Dn)*(1-g1)*g2
new new_unsure (1-Dn)*g1
new new_new Dn
new new_new (1-Dn)*(1-g1)*(1-g2)"
  m2_tb <- traitMPT(model, skkagg_g, restrictions = list("Do = Dn"))
  summ2tb <- summary(m2_tb)

  u2htsm_model2 <- make_mpt(EQNFILE, restrictions = list("Do = Dn"))
  u2htsm_model2
  m2 <- mpt(resp ~ 1 + (1|s|id), skk_red, model = u2htsm_model2,
            tree = "type", silent = 2, refresh = 0, open_progress = FALSE)
  summ2 <- summary(m2)
  #str(summ2)
  expect_equal(summ2$fixed$Estimate,
               summ2tb$groupParameters$mu[,"Mean"],
               tolerance = 0.1, ignore_attr = TRUE)

  expect_equal(summ2$random$id$Estimate,
               c(summ2tb$groupParameters$sigma[,"Mean"], summ2tb$groupParameters$rho[,"Mean"]),
               tolerance = 0.1, ignore_attr = TRUE)

  #m2
  ppp2 <- ppp_test(m2)
  #expect_equal(unname(ppp1[1]), 0.075, tolerance = 0.05)
})
