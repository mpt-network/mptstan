test_that("can create stancode", {
  EQNFILE <- system.file("extdata", "u2htm.eqn", package = "mptstan")
  suppressMessages(expect_warning(u2htsm_model <- make_mpt(EQNFILE),
                 "parameter names ending with a number amended with 'x'"))
  mptformula <- mpt_formula(resp ~ race, model = u2htsm_model)
  code_simple <- stancode(mptformula, data = skk13, tree = "type")

  default_inter <- c("normal_lpdf", "0, 1")
  default_slope <- c("normal_lpdf", "0, 0.5")
  expect_match(code_simple, paste0("lprior += ", default_inter[1], "(",
               "Intercept", " | ", default_inter[2], ");"),
               fixed = TRUE)

  expect_match(code_simple, paste0("lprior += ", default_inter[1], "(",
                                   "Intercept", " | ", default_inter[2], ");"),
               fixed = TRUE)
  expect_match(code_simple, paste0("lprior += ", default_inter[1], "(",
                                   "Intercept_Do", " | ", default_inter[2], ");"),
               fixed = TRUE)
  expect_match(code_simple, paste0("lprior += ", default_inter[1], "(",
                                   "Intercept_g1x", " | ", default_inter[2], ");"),
               fixed = TRUE)
  expect_match(code_simple, paste0("lprior += ", default_inter[1], "(",
                                   "Intercept_g2x", " | ", default_inter[2], ");"),
               fixed = TRUE)
    expect_match(code_simple, paste0("lprior += ", default_slope[1], "(",
                                   "b", " | ", default_slope[2], ");"),
               fixed = TRUE)
  expect_match(code_simple, paste0("lprior += ", default_slope[1], "(",
                                   "b_Do", " | ", default_slope[2], ");"),
               fixed = TRUE)
  expect_match(code_simple, paste0("lprior += ", default_slope[1], "(",
                                   "b_g1x", " | ", default_slope[2], ");"),
               fixed = TRUE)
  expect_match(code_simple, paste0("lprior += ", default_slope[1], "(",
                                   "b_g2x", " | ", default_slope[2], ");"),
               fixed = TRUE)

  ##### change prior:
  code_prior <- stancode(mptformula, data = skk13, tree = "type",
                         default_prior_intercept = "student(0, 2)",
                         default_prior_coef = "cauchy(0, 1)")

  default_inter <- c("student_lpdf", "0, 2")
  default_slope <- c("cauchy_lpdf", "0, 1")
  expect_match(code_prior, paste0("lprior += ", default_inter[1], "(",
                                   "Intercept", " | ", default_inter[2], ");"),
               fixed = TRUE)

  expect_match(code_prior, paste0("lprior += ", default_inter[1], "(",
                                   "Intercept", " | ", default_inter[2], ");"),
               fixed = TRUE)
  expect_match(code_prior, paste0("lprior += ", default_inter[1], "(",
                                   "Intercept_Do", " | ", default_inter[2], ");"),
               fixed = TRUE)
  expect_match(code_prior, paste0("lprior += ", default_inter[1], "(",
                                   "Intercept_g1x", " | ", default_inter[2], ");"),
               fixed = TRUE)
  expect_match(code_prior, paste0("lprior += ", default_inter[1], "(",
                                   "Intercept_g2x", " | ", default_inter[2], ");"),
               fixed = TRUE)
  expect_match(code_prior, paste0("lprior += ", default_slope[1], "(",
                                   "b", " | ", default_slope[2], ");"),
               fixed = TRUE)
  expect_match(code_prior, paste0("lprior += ", default_slope[1], "(",
                                   "b_Do", " | ", default_slope[2], ");"),
               fixed = TRUE)
  expect_match(code_prior, paste0("lprior += ", default_slope[1], "(",
                                   "b_g1x", " | ", default_slope[2], ");"),
               fixed = TRUE)
  expect_match(code_prior, paste0("lprior += ", default_slope[1], "(",
                                   "b_g2x", " | ", default_slope[2], ");"),
               fixed = TRUE)

})

test_that("single tree model produces code", {
  skip_if_not_installed("MPTinR")
  model1 <- system.file("extdata", "rb.fig1.model", package = "MPTinR")
  om1 <- make_mpt(model1, categories = c("BC", "BnC", "nBC", "nBnC"))
  smform <- mpt_formula(resp_long ~ test, model = om1)
  #load("tests/testthat/riefer-batchelder-1988-fig1-data.rda")
  load("riefer-batchelder-1988-fig1-data.rda")
  scode <- stancode(smform, data = drb_fig1)
  expect_match(scode, "    prob[1] = p*q*r;
    prob[2] = p*q*(1-r);
    prob[3] = p*(1-q)*r;
    prob[4] = p*(1-q)*(1-r) + (1-p);", fixed = TRUE)

})
