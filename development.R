library("devtools")
load_all()

document()
check()

devtools::build_readme()

### example

str(skk13)

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
s2htsm_model
print(s2htsm_model, eqn = TRUE)
str(s2htsm_model, 1)
cat(s2htsm_model$brms_llk)
s2htsm_model$family
str(s2htsm_model$family)
s2htsm_model$names$trees

s2htm_formula <- mpt_formula(
  resp ~ race + (race|s|id) + (1|p|stim),
  model = s2htsm_model
)
s2htm_formula2 <- mpt_formula(
  Do ~ race + (race|s|id) + (1|p|stim),
  Dn ~ race + (race|s|id) + (1|p|stim),
  g1x ~ race + (race|s|id) + (1|p|stim),
  g2x ~ race + (race|s|id) + (1|p|stim),
  response = ~ resp,
  model = s2htsm_model
)
all.equal(s2htm_formula, s2htm_formula2)

class(s2htm_formula)

sdat <- standata(s2htm_formula, data = skk13, tree = "type")
str(sdat, 1)
data_prep <- prep_data(formula = s2htm_formula,
                       data = skk13, tree = "type")
head(data_prep)

stancode(s2htm_formula, data = skk13, tree = "type")

fit1 <- mpt(s2htm_formula, data = skk13, tree = "type",
            cores = 4, warmup = 500, iter = 1000)
summary(fit1)
emmeans::emmeans(fit1, "race", type = "response")

emmeans::emmeans(fit1, "race", type = "response", dpar = "Do")
emmeans::emmeans(fit1, "race", type = "response", dpar = "g1x")
emmeans::emmeans(fit1, "race", type = "response", dpar = "g2x")

qfit1 <- mpt(s2htm_formula, data = skk13, tree = "type",
            algorithm = "fullrank")

qfit2 <- mpt(resp ~ race + (race|s|id), data = skk13, tree = "type",
             model = s2htsm_model, algorithm = "fullrank", init = "0")
qfit2
qfit2$data <- skk13
emmeans::emmeans(qfit2, "race", type = "response", data = skk13)

fit2 <- mpt(resp ~ race, data = skk13, model = s2htsm_model,
            tree = "type",
            cores = 4, warmup = 200, iter = 500)
fit2

loo1 <- loo(fit2)
loo1b <- loo(fit2, moment_match = TRUE)

pp_check(fit2, type = "bars_grouped", group = "mpt_tree",
         ndraws = 100)

peprd <- posterior_epred(fit2)
peprd[1, 1:20,]

conditional_effects(fit2)

head(skk13)

s2htm_formula3 <- mpt_formula(
  Do ~ race ,
  Dn ~ race ,
  g1x ~ 1,
  g2x ~ race,
  response = ~ resp,
  model = s2htsm_model
)
fit3 <- mpt(s2htm_formula3, data = skk13, model = s2htsm_model,
            tree = "type",
            cores = 4, warmup = 200, iter = 500)
loo3b <- loo(fit3, moment_match = TRUE)

lc <- loo_compare(loo1b, loo3b)
lc

####
model1 <- system.file("extdata", "rb.fig1.model", package = "MPTinR")
om1 <- make_mpt(model1, categories = c("BC", "BnC", "nBC", "nBnC"))
print(om1, eqn = TRUE)

data(rb.fig1.data, package = "MPTinR")
drb1 <- rb.fig1.data
colnames(drb1) <- c("BC", "BnC", "nBC", "nBnC")
rownames(drb1) <- c(paste0("test1.", 1:3),
                    paste0("test2.", 1:3))

library("tidyverse")
drb1_use <- drb1 %>%
  as.data.frame() %>%
  rownames_to_column(var = "test") %>%
  pivot_longer(cols = BC:nBnC) %>%
  mutate(resp_long = map2(name, value, ~rep(.x, .y))) %>%
  unnest(resp_long)



fit_n <- mpt(resp_long ~ test, data = drb1_use, model = om1,
             cores = 3, chains = 3)
fit_n
fit_n$model

get_prior(fit_n)
pall <- set_prior("normal(0, 1)", class = "b") +
  set_prior("normal(0, 1)", class = "Intercept")
pinter <- set_prior("normal(0, 1)", class = "Intercept")

fit_n2 <- mpt(resp_long ~ test, data = drb1_use, model = om1,
              prior = pinter, cores = 3, chains = 3)
fit_n2
fit_n2$model

ff2 <- mpt_formula(resp_long ~ test, model = om1,
                   brms_args = list(center = FALSE))
get_prior(ff2$brmsformula, data = drb1_use, prior = pinter)
default_prior(ff2$brmsformula, data = drb1_use)
stancode(ff2, data = drb1_use)
pinter2 <- set_prior("normal(0, 0.5)", class = "b") +
  set_prior("normal(0, 1)", class = "b", coef = "Intercept") +
  set_prior("normal(0, 0.5)", class = "b", dpar = "q") +
  set_prior("normal(0, 1)", class = "b", coef = "Intercept", dpar = "q") +
  set_prior("normal(0, 0.5)", class = "b", dpar = "r") +
  set_prior("normal(0, 1)", class = "b", coef = "Intercept", dpar = "r")
stancode(ff2, data = drb1_use, prior = pinter2)

fit_n3 <- mpt(ff2, data = drb1_use, model = om1,
              prior = pinter2, cores = 3, chains = 3)
fit_n3

emmeans::emmeans(fit_n3, "test", type = "response")
emmeans::emmeans(fit_n3, "test", type = "response", dpar = "q")
emmeans::emmeans(fit_n3, "test", type = "response", dpar = "r")

pinter3 <- set_prior("normal(0, 1)", class = "b") +
  set_prior("normal(0, 1)", class = "b", coef = "Intercept") +
  set_prior("normal(0, 1)", class = "b", dpar = "q") +
  set_prior("normal(0, 1)", class = "b", coef = "Intercept", dpar = "q") +
  set_prior("normal(0, 1)", class = "b", dpar = "r") +
  set_prior("normal(0, 1)", class = "b", coef = "Intercept", dpar = "r")
stancode(ff2, data = drb1_use, prior = pinter2)

fit_n4 <- mpt(ff2, data = drb1_use, model = om1, cores = 3, chains = 3)
fit_n4
emmeans::emmeans(fit_n4, "test", type = "response")
emmeans::emmeans(fit_n4, "test", type = "response", dpar = "q")
emmeans::emmeans(fit_n4, "test", type = "response", dpar = "r")

##### usethis stuff
usethis::use_package("extraDistr", type = "Imports")
usethis::use_package("emmeans", type = "Suggests")
usethis::use_data(skk13)
usethis::use_readme_rmd()

