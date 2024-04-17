
## Model with 4 parameters: Dn, Do, g1x, g2x
EQNFILE <- system.file("extdata", "u2htm.eqn", package = "mptstan")
u2htsm_model <- make_mpt(EQNFILE)
u2htsm_model

## formulas are given for following data
str(skk13)

#### simplest possible formula: ~ 1
## no random-effects and there is only one set of parameters (i.e., no
## differences across conditions).
## Same model holds for all MPT model parameters
(f1 <- mpt_formula(resp ~ 1, model = u2htsm_model))

#### model with condition effects: ~ race
## Each parameter differs across the race variable
(f2 <- mpt_formula(resp ~ race, model = u2htsm_model))

### model with simple by-participant random effects
## because race is within-subject factor, we need random slopes for race
## this model only has correlations within one MPT model parameter
(f3 <- mpt_formula(resp ~ race + (race|id), model = u2htsm_model))

### model with correlated by-participant random effects
## to employ full latent-trait structure (Klauer, 2010), we need to have
## correlations across MPT model parameters
(f4 <- mpt_formula(resp ~ race + (race|p|id), model = u2htsm_model))

### model with crossed random-effects for participants and items:
## because race is a between-item factor (i.e., race is nested within) the item
## factor, we only have random intercepts for item, but they are correlated as
## well.
(f5 <- mpt_formula(resp ~ race + (race|p|id) + (1|i|item), model = u2htsm_model))

### we can also specify an individual structure for each parameters.
## In this case, we need to specify the response variable separately.
(f6 <- mpt_formula(
  Do ~ race + (race|p|id) + (1|i|item),
  Dn ~ race + (race|p|id) + (1|i|item),
  g1x ~ race + (race|p|id) + (1|i|item),
  g2x ~ race + (race|p|id) + (1|i|item),
  response = ~ resp,
  model = u2htsm_model))

all.equal(f5, f6) ## TRUE

### can be more interesting, if we want different structures for each parameter
(f7 <- mpt_formula(
  Do ~ 1 + (1|p|id) + (1|i|item),
  Dn ~ race + (race|p|id) + (1|i|item),
  g1x ~ 1 + (1|p|id) + (1|i|item),
  g2x ~ race + (race|p|id) + (1|i|item),
  response = ~ resp,
  model = u2htsm_model))
