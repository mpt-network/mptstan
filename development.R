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

s2htsm_model <- make_mpt(text = s2htm, type = "easy", trees = c("old", "new"),
         categories = rep(c("old", "unsure", "new"), 2))
s2htsm_model
print(s2htsm_model, eqn = TRUE)
str(s2htsm_model, 1)
cat(s2htsm_model$brms_llk)
s2htsm_model$family

s2htm_formula <- mptformula(
  resp ~ race + (race|s|id) + (1|p|stim),
  model = s2htsm_model
)
s2htm_formula2 <- mptformula(
  Do ~ race + (race|s|id) + (1|p|stim),
  Dn ~ race + (race|s|id) + (1|p|stim),
  g1x ~ race + (race|s|id) + (1|p|stim),
  g2x ~ race + (race|s|id) + (1|p|stim),
  response = ~ resp,
  model = s2htsm_model
)

##### usethis stuff
usethis::use_package("stringr", type = "Imports")
usethis::use_data(skk13)

