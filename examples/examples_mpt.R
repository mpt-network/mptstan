\dontrun{
### Step 1: Specify model using make_mpt()
## model in easy format (with model specified as text)
## unsure-extended 2-high threshold model for recognition memory
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

# need to specify tree names and category names for easy format
u2htsm_model <- make_mpt(text = u2htm, trees = c("old", "new"),
                         categories = rep(c("old", "unsure", "new"), 2))
u2htsm_model


### Step 2: Fit model with same formula for all model parameters

## we fit to data from Singmann, Kellen, & Klauer (2013, CogSci-Proc):
# Investigating the Other-Race Effect of Germans towards Turks and Arabs using
# Multinomial Processing Tree Models
# http://singmann.org/download/publications/SKK-CogSci2013.pdf
str(skk13)

## here we use simplified syntax without any random effects for fitting speed.
fit_fast <- mpt(resp ~ race, data = skk13, model = u2htsm_model,
                tree = "type",
                cores = min(c(4, parallel::detectCores()))) ## uses multicore
## a more appropriate formula would be: resp ~ race + (race|s|id) + (1|p|stim)
## (i.e., crossed-random effects and full correlations among random terms)

### Step 3: Inspect results
fit_fast # in output, no parameter name refers to first MPT parameter (here: Dn)

## Check model fit
ppp_test(fit_fast)

## we can use package emmeans for marginal effects of conditions for parameters
# Default is first MPT model parameter (here: Dn)
emmeans::emmeans(fit_fast, "race", type = "response")
# using the dpar argument, we can get output for any other model parameter
emmeans::emmeans(fit_fast, "race", type = "response", dpar = "Do")
emmeans::emmeans(fit_fast, "race", type = "response", dpar = "g1x")
emmeans::emmeans(fit_fast, "race", type = "response", dpar = "g2x")

## We can also use all brms post-processing
# information criteria (e.g., loo)
(loo_model <- loo(fit_fast))
# posterior predictive checks
pp_check(fit_fast, type = "bars_grouped", group = "mpt_tree", ndraws = 100)
# get posterior mean predictions
pepred <- posterior_epred(fit_fast)
str(pepred)  ## dimensions are: samples, observations, probability (in tree)

### Alternative Step 2: First specify formula, then fit model
## Step 2a: Specify formula for each parameter separately using mpt_formula()
## (formula could be different for each model parameter)
u2htm_formula <- mpt_formula(
  Do ~ race + (race|s|id) + (1|p|stim),
  Dn ~ race + (race|s|id) + (1|p|stim),
  g1x ~ race + (race|s|id) + (1|p|stim),
  g2x ~ race + (race|s|id) + (1|p|stim),
  response = ~ resp,
  model = u2htsm_model
)
u2htm_formula

## Step 2b: Fit model using formula (takes rather long)
fit_slow <- mpt(u2htm_formula, data = skk13,
                tree = "type",
                cores = min(c(4, parallel::detectCores()))) ## uses multicore
fit_slow
}
