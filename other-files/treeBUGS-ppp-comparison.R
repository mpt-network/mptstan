library("tidyverse")
library("TreeBUGS")
EQNFILE <- system.file("extdata", "u2htm.eqn", package = "mptstan")

skk13 <- mptstan::skk13
head(skk13)

skkagg <- skk13 %>%
  count(id, race, type, resp, .drop = FALSE) %>%
  pivot_wider(names_from = c(type, resp), values_from = n)

cat(readLines(EQNFILE), sep = "\n")

skkagg_g <- skkagg %>%
  filter(race == "german")
skkagg_g

model <- "8
old old_old Do
old old_old (1-Do)*(1-g1)*g2
old old_unsure (1-Do)*g1
old old_new (1-Do)*(1-g1)*(1-g2)
new new_old (1-Dn)*(1-g1)*g2
new new_unsure (1-Dn)*g1
new new_new Dn
new new_new (1-Dn)*(1-g1)*(1-g2)"

m1_tb <- traitMPT(model, skkagg_g)
PPP(m1_tb)
#  ## Mean structure (T1):
#  Observed =  0.08551339 ; Predicted =  0.08307861 ; p-value =  0.477
#
# ## Covariance structure (T2):
#  Observed =  16.7188 ; Predicted =  14.65547 ; p-value =  0.431

m2_tb <- traitMPT(model, skkagg_g, restrictions = list("Do = Dn"))
summary(m2_tb)
# Call:
# traitMPT(eqnfile = model, data = skkagg_g, restrictions = list("Do = Dn"))
#
# Group-level medians of MPT parameters (probability scale):
#          Mean    SD  2.5%   50% 97.5% Time-series SE n.eff  Rhat R_95%
# mean_Dn 0.497 0.033 0.431 0.497 0.562          0.001   609 1.002 1.005
# mean_g1 0.091 0.028 0.044 0.088 0.152          0.002   307 1.028 1.042
# mean_g2 0.389 0.045 0.304 0.387 0.484          0.002   373 1.023 1.071
#
# Mean/Median of latent-trait values (probit-scale) across individuals:
#                Mean    SD   2.5%    50%  97.5% Time-series SE n.eff  Rhat R_95%
# latent_mu_Dn -0.007 0.083 -0.173 -0.006  0.157          0.003   609 1.002 1.005
# latent_mu_g1 -1.356 0.170 -1.701 -1.355 -1.027          0.010   318 1.016 1.022
# latent_mu_g2 -0.284 0.118 -0.512 -0.287 -0.041          0.006   376 1.022 1.069
#
# Standard deviation of latent-trait values (probit scale) across individuals:
#                  Mean    SD  2.5%   50% 97.5% Time-series SE n.eff  Rhat R_95%
# latent_sigma_Dn 0.490 0.073 0.365 0.484 0.651          0.001  3306 1.000 1.000
# latent_sigma_g1 1.021 0.161 0.756 1.004 1.385          0.004  1814 1.031 1.101
# latent_sigma_g2 0.682 0.101 0.508 0.674 0.907          0.002  2620 1.003 1.009
#
# Correlations of latent-trait values on probit scale:
#              Mean    SD   2.5%    50% 97.5% Time-series SE n.eff  Rhat R_95%
# rho[Dn,g1] -0.062 0.178 -0.406 -0.061 0.283          0.003  3996 1.000 1.001
# rho[Dn,g2] -0.034 0.176 -0.372 -0.033 0.307          0.003  4029 1.002 1.008
# rho[g1,g2]  0.230 0.170 -0.122  0.239 0.541          0.003  3676 1.002 1.009
str(summary(m2_tb))
summ2tb <- summary(m2_tb)
summ2tb$groupParameters

PPP(m2_tb)
#  ## Mean structure (T1):
#  Observed =  0.200318 ; Predicted =  0.08652024 ; p-value =  0.075
#
# ## Covariance structure (T2):
#  Observed =  50.68244 ; Predicted =  14.82402 ; p-value =  0.006
