library("devtools")
load_all()

document()

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

##### usethis stuff
usethis::use_package("extraDistr", type = "Imports")
usethis::use_data(skk13)

