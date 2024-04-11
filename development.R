library("devtools")
load_all()

usethis::use_package("stringr", type = "Imports")
usethis::use_data(skk13)

### example

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

mod_df <- read_mpt(text = s2htm, type = "easy", trees = c("old", "new"),
         categories = rep(c("old", "skip", "new"), 2))
mod_list <- parse_model_df(mod_df)
mod_list
check.MPT.probabilities(model_list = mod_list)
mod_code <- make_llk_function(mod_df)
cat(mod_code)
