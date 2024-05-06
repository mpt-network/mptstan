test_that("basic make_mpt examples work", {
  u2htm <- "
# Old Items
Do + (1 - Do) * (1 - g1) * g2
(1 - Do) * g1
(1 - Do) * (1 - g1) * (1 - g2)

# New Items
(1 - Dn) * (1 - g1) * g2
(1 - Dn) * g1
Dn + (1 - Dn) * (1 - g1) * (1 - g2)
"

  # need to specify tree names and category names
  expect_warning(u2htsm_model <- make_mpt(text = u2htm,
                                          trees = c("old", "new"),
                                          categories = rep(c("old", "unsure", "new"), 2)),
                 "parameter names ending with a number amended with 'x'")

  expect_s3_class(u2htsm_model, "mpt_model")

  EQNFILE <- system.file("extdata", "u2htm.eqn", package = "mptstan")
  suppressMessages(expect_warning(u2htsm_model_b <- make_mpt(EQNFILE),
                 "parameter names ending with a number amended with 'x'"))

  ### remove as environments, as they don't need to be the same
  u2htsm_model$family$env <- NULL
  u2htsm_model_b$family$env <- NULL
  expect_equal(u2htsm_model, u2htsm_model_b)
})

test_that("single tree model works", {
  skip_if_not_installed("MPTinR")
  model1 <- system.file("extdata", "rb.fig1.model", package = "MPTinR")
  om1 <- make_mpt(model1, categories = c("BC", "BnC", "nBC", "nBnC"))

  expect_s3_class(om1, "mpt_model")
  expect_identical(om1$parameters, c("p", "q", "r"))
})
