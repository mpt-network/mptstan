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

# s2htm_formula <- c(
#   Do ~
# )

s2htsm_model <- make_mpt(text = s2htm, type = "easy", trees = c("old", "new"),
         categories = rep(c("old", "unsure", "new"), 2))
s2htsm_model
print(s2htsm_model, eqn = TRUE)

##### usethis stuff
usethis::use_package("stringr", type = "Imports")
usethis::use_data(skk13)

