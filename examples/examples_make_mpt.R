
## read-in model in easy format (with model specified as text)
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

# need to specify tree names and category names
u2htsm_model <- make_mpt(text = u2htm,
                         trees = c("old", "new"),
                         categories = rep(c("old", "unsure", "new"), 2))
u2htsm_model

# print model with eqn style
print(u2htsm_model, eqn = TRUE)

# brms family object
u2htsm_model$family

## write model as eqn file (add empty line on top):
\dontrun{
write.table(u2htsm_model$df, file = "u2htm.eqn",
           quote = FALSE, row.names = FALSE, col.names = FALSE)
}

## create same model, but from EQN file
EQNFILE <- system.file("extdata", "u2htm.eqn", package = "mptstan")

u2htsm_model_b <- make_mpt(EQNFILE)
u2htsm_model_b
