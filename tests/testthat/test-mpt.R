
test_that("mpt() leads to similar results regardless of aggregation and probability / log probability scale", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  library(dplyr)
  library(tidyr)

  skk13 <- mptstan::skk13
  # 1. Non-aggregated Data - prob scale
  options(mc.cores = 2)
  u2htsm_model <- suppressWarnings(make_mpt(system.file("extdata", "u2htm.eqn",
                                                        package = "mptstan")))
  u2htm_formula <- mpt_formula(resp ~ 1 + (1 | id),
                               model = u2htsm_model,
                               data_format = "long")
  fit_skk <- mpt(u2htm_formula, data = skk13 |>
                   dplyr::filter(race == "german"),
                 tree = "type",
                 init_r = 0.5,
                 log_p = FALSE,
                 chains = 2,
                 refresh = 0,
                 open_progress = FALSE
  )

  # 2. Aggregated Data - prob scale
  skkagg <- skk13 |>
    dplyr::filter(race == "german") |>
    dplyr::count(resp, id, type) |>
    tidyr::pivot_wider(names_from = resp, values_from = n, values_fill = 0)

  u2htm_formula_agg <- mpt_formula(~ 1 + (1 | id),
                               model = u2htsm_model, data_format = "wide")

  fit_skk_agg <- mpt(u2htm_formula_agg, data = skkagg,
                 tree = "type",
                 init_r = 0.5,
                 chains = 2,
                 refresh = 0, open_progress = FALSE
  )

  # 3. Aggregated data - log scale
  fit_skk_log <- mpt(u2htm_formula, data = skk13 %>% dplyr::filter(race == "german"),
                 tree = "type",
                 init_r = 0.5,
                 log_p = TRUE,
                 chains = 2,
                 refresh = 0, open_progress = FALSE
  )

  # 4. Aggregated Data - log scale
  fit_skk_agg_log <- mpt(u2htm_formula_agg, data = skkagg,
                     tree = "type",
                     init_r = 0.5,
                     log_p = TRUE,
                     chains = 2,
                     refresh = 0, open_progress = FALSE
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
