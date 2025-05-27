
test_that("mpt() leads to similar results regardless of aggregation and probability / log probability scale", {
  skip_if_not_installed("tidyverse")
  library(tidyverse)

  skk13 <- mptstan::skk13
  # 1. Non-aggregated Data - prob scale
  options(mc.cores = parallel::detectCores())
  u2htsm_model <- make_mpt("inst/extdata/u2htm.eqn")
  u2htm_formula <- mpt_formula(resp ~ 1 + (1 | id),
                               model = u2htsm_model,
                               data_format = "long")
  fit_skk <- mpt(u2htm_formula, data = skk13 %>% dplyr::filter(race == "german"),
                   tree = "type",
                   init_r = 0.5,
                 log_p = FALSE
  )

  # 2. Aggregated Data - prob scale
  skkagg <- skk13 %>%
    filter(race == "german") %>%
    count(resp, id, type) %>%
    pivot_wider(names_from = resp, values_from = n)
  skkagg[is.na(skkagg)] <- 0

  u2htm_formula_agg <- mpt_formula(~ 1 + (1 | id),
                               model = u2htsm_model, data_format = "wide")

  fit_skk_agg <- mpt(u2htm_formula_agg, data = skkagg,
                 tree = "type",
                 init_r = 0.5
  )

  # 3. Aggregated data - log scale
  fit_skk_log <- mpt(u2htm_formula, data = skk13 %>% dplyr::filter(race == "german"),
                 tree = "type",
                 init_r = 0.5,
                 log_p = TRUE
  )

  # 4. Aggregated Data - log scale
  fit_skk_agg_log <- mpt(u2htm_formula_agg, data = skkagg,
                     tree = "type",
                     init_r = 0.5,
                     log_p = TRUE
  )

  expect_equal(fixef(fit_skk)[, 1], fixef(fit_skk_agg)[, 1], tolerance = 1e-1)
  expect_equal(fixef(fit_skk)[, 2], fixef(fit_skk_agg)[, 2], tolerance = 1e-1)

  expect_equal(fixef(fit_skk)[, 1], fixef(fit_skk_log)[, 1], tolerance = 1e-1)
  expect_equal(fixef(fit_skk)[, 2], fixef(fit_skk_log)[, 2], tolerance = 1e-1)

  expect_equal(fixef(fit_skk)[, 1], fixef(fit_skk_agg_log)[, 1], tolerance = 1e-1)
  expect_equal(fixef(fit_skk)[, 2], fixef(fit_skk_agg_log)[, 2], tolerance = 1e-1)

  expect_equal(ranef(fit_skk)$id[, 1, ], ranef(fit_skk_agg)$id[, 1, ], tolerance = 1e-1)
  expect_equal(ranef(fit_skk)$id[, 1, ], ranef(fit_skk_log)$id[, 1, ], tolerance = 1e-1)
  expect_equal(ranef(fit_skk)$id[, 1, ], ranef(fit_skk_agg_log)$id[, 1, ], tolerance = 1e-1)

})


test_that("mpt() works the same for a given mpt_formula and a formula", {
  skk13 <- mptstan::skk13
  # 1. Non-aggregated Data - prob scale
  options(mc.cores = parallel::detectCores())
  u2htsm_model <- make_mpt("inst/extdata/u2htm.eqn")
  u2htm_formula <- mpt_formula(resp ~ 1 + (1 | id),
                               model = u2htsm_model,
                               data_format = "long")
  fit_skk <- mpt(u2htm_formula, data = skk13 %>% dplyr::filter(race == "german"),
                 tree = "type",
                 init_r = 0.5, seed = 2
  )

  fit_skk_2 <- mpt(resp ~ 1 + (1 | id), model = u2htsm_model,
                   data_format = "long", tree = "type",
                   data = skk13,
                   init_r = 0.5, seed = 2)

  # 2. Aggregated data
  skkagg <- skk13 %>%
    filter(race == "german") %>%
    count(resp, id, type) %>%
    pivot_wider(names_from = resp, values_from = n)
  skkagg[is.na(skkagg)] <- 0

  u2htm_formula <- mpt_formula(resp ~ 1 + (1 | id),
                               model = u2htsm_model,
                               data_format = "wide")

  fit_skk <- mpt(u2htm_formula, data = skk13agg,
                 tree = "type",
                 init_r = 0.5,
                 seed = 2
  )

  fit_skk_2 <- mpt(resp ~ 1 + (1 | id), model = u2htsm_model,
                   data_format = "long", tree = "type",
                   data = skk13,
                   init_r = 0.5,
                   seed = 2)
  expect_equal(fixef(fit_skk)[, 1], fixef(fit_skk_agg)[, 1], tolerance = 1e-1)
  expect_equal(fixef(fit_skk)[, 2], fixef(fit_skk_agg)[, 2], tolerance = 1e-1)

  expect_equal(fixef(fit_skk)[, 1], fixef(fit_skk_log)[, 1], tolerance = 1e-1)
  expect_equal(fixef(fit_skk)[, 2], fixef(fit_skk_log)[, 2], tolerance = 1e-1)

  expect_equal(fixef(fit_skk)[, 1], fixef(fit_skk_agg_log)[, 1], tolerance = 1e-1)
  expect_equal(fixef(fit_skk)[, 2], fixef(fit_skk_agg_log)[, 2], tolerance = 1e-1)

  expect_equal(ranef(fit_skk)$id[, 1, ], ranef(fit_skk_agg)$id[, 1, ], tolerance = 1e-1)
  expect_equal(ranef(fit_skk)$id[, 1, ], ranef(fit_skk_log)$id[, 1, ], tolerance = 1e-1)
  expect_equal(ranef(fit_skk)$id[, 1, ], ranef(fit_skk_agg_log)$id[, 1, ], tolerance = 1e-1)
})
