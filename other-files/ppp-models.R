str(skk13)

options(mc.cores = parallel::detectCores())
library("tidyverse")
skk_g <- skk13 |>
  filter(race == "german")

EQNFILE <- system.file("extdata", "u2htm.eqn", package = "mptstan")
u2htsm_model2 <- make_mpt(EQNFILE, restrictions = list("Do = Dn"))
u2htsm_model2
m2 <- mpt(resp ~ 1 + (1|s|id), skk_g, model = u2htsm_model2,
          tree = "type", silent = 2, refresh = 0, open_progress = FALSE)
mpt_emmeans(m2, "1")
# parameter X1 response lower.HPD upper.HPD parameter.1
# Dn        .     0.499    0.4315     0.560 Dn
# g1x       .     0.086    0.0376     0.141 g1x
# g2x       .     0.387    0.3011     0.467 g2x

ppp_test(m2, ndraws = 4000)
# p.value   ll_model    ll_data
#  0.4755 -2576.2398 -2772.5431


s2htm <- "
# Old Items
Do + (1 - Do) * (1 - g1) * g2
(1 - Do) * g1
(1 - Do) * (1 - g1) * (1 - g2)

# New Items
(1 - Dn) * (1 - g1) * g2
(1 - Dn) * g1
Dn + (1 - Dn) * (1 - g1) * (1 - g2)
"

s2htsm_model <- make_mpt(text = s2htm, type = "easy",
                         trees = c("old", "new"),
                         categories = rep(c("old", "unsure", "new"), 2))



s2htm_formula <- mpt_formula(
  resp ~ 1 + (1|s|id),
  model = s2htsm_model
)

fit_1 <- mpt(s2htm_formula, data = skk_g, tree = "type")
mpt_emmeans(fit_1, "1")
# parameter X1 response lower.HPD upper.HPD parameter.1
# Dn        .    0.4582     0.305     0.582 Dn
# Do        .    0.5318     0.438     0.617 Do
# g1x       .    0.0849     0.041     0.141 g1x
# g2x       .    0.3577     0.274     0.439 g2x

ppp_test(fit_1)


