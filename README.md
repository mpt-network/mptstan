
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mptstan

<!-- badges: start -->
<!-- badges: end -->

Estimate multinomial processing tree (MPT) models, a model class popular
in cognitive psychology, in a Bayesian framework using package `brms`
(i.e., ultimately `Stan`). This allows specifying models with any type
of multivariate normal hierarchical or multilevel structure, including
models with single and multiple random effects (i.e., crossed random
effects). Model specification can be tailored for each model parameter.

## Installation

You can install the development version of `mptstan` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mpt-network/mptstan")
```

## Example

We show the analysis of a recognition memory data set (from Singmann,
Kellen, & Klauer, 2013) using the unsure-extended 2-high threshold model
to a dataset investigating the other-race effect (i.e., a study with two
different types of old and new items, own-race faces and other-race
faces). This data is available in `mptstan` as `skk13`. We will analyse
this data using crossed-random effects for participamnts and items.

``` r
library(mptstan)
str(skk13)
#> 'data.frame':    8400 obs. of  7 variables:
#>  $ id   : Factor w/ 42 levels "1","3","5","6",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ trial: Factor w/ 200 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ race : Factor w/ 2 levels "german","arabic": 2 1 1 1 1 1 1 2 1 1 ...
#>  $ type : Factor w/ 2 levels "old","new": 1 1 2 2 1 1 1 1 2 2 ...
#>  $ resp : Factor w/ 3 levels "old","unsure",..: 3 1 3 1 1 1 3 1 3 3 ...
#>  $ rt   : num  4.68 2.75 4.25 1.6 0.95 ...
#>  $ stim : Factor w/ 200 levels "A001","A002",..: 40 132 117 143 140 162 193 19 120 170 ...
```

### Step 1: Create MPT Model Object

The first step when using `mptstan` is the creation of a MPT model
object using `make_mpt()` (which creates an object of class
`mpt_model`).

`make_mpt()` can read MPT models in both the commonly used `EQN` model
format (e.g., used by `TreeBUGS`) and the `easy` format introduced by
`MPTinR`.

``` r
# For the easy EQN format, we just need the EQN file location:
EQNFILE <- system.file("extdata", "u2htm.eqn", package = "mptstan")
u2htsm_model <- make_mpt(EQNFILE) ## make_mpt() auto-detects EQN files from name
#> model type auto-detected as 'eqn'
#> Warning: parameter names ending with a number amended with 'x'
u2htsm_model
#> 
#> MPT model with 4 independent categories (from 2 trees) and 4 parameters:
#>   Dn, Do, g1x, g2x
#> 
#> Tree 1: old
#>   Categories: old, unsure, new 
#>   Parameters: Do, g1x, g2x
#> Tree 2: new
#>   Categories: old, unsure, new 
#>   Parameters: Dn, g1x, g2x

## Alternatively, we can just enter the equations and use the easy format.
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
# for the easy format, we need to specify tree names and category names
u2htsm_model_2 <- make_mpt(text = u2htm, 
                           trees = c("old", "new"),
                           categories = rep(c("old", "unsure", "new"), 2))
#> Warning: parameter names ending with a number amended with 'x'
u2htsm_model_2
#> 
#> MPT model with 4 independent categories (from 2 trees) and 4 parameters:
#>   Dn, Do, g1x, g2x
#> 
#> Tree 1: old
#>   Categories: old, unsure, new 
#>   Parameters: Do, g1x, g2x
#> Tree 2: new
#>   Categories: old, unsure, new 
#>   Parameters: Dn, g1x, g2x
```

### Step 2: Create Formula (Optional)

``` r
u2htm_formula <- mpt_formula(resp ~ race + (race|s|id) + (1|p|stim), 
                             model = u2htsm_model)
```

## Step 3: Fit Model

``` r
fit_slow <- mpt(u2htm_formula, data = skk13,
                tree = "type",
                cores = min(c(4, parallel::detectCores()))) ## uses multicore
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
```

``` r
fit_slow
#>  Family: mpt 
#>   Links: mu = probit; Do = probit; g1x = probit; g2x = probit 
#> Formula: resp ~ race + (race | s | id) + (1 | p | stim) 
#>          Do ~ race + (race | s | id) + (1 | p | stim)
#>          g1x ~ race + (race | s | id) + (1 | p | stim)
#>          g2x ~ race + (race | s | id) + (1 | p | stim)
#>    Data: structure(list(id = structure(c(1L, 1L, 1L, 1L, 1L (Number of observations: 8400) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
#>          total post-warmup draws = 4000
#> 
#> Multilevel Hyperparameters:
#> ~id (Number of levels: 42) 
#>                                    Estimate Est.Error l-95% CI u-95% CI Rhat
#> sd(Intercept)                          0.76      0.18     0.46     1.14 1.00
#> sd(racearabic)                         0.22      0.17     0.01     0.66 1.00
#> sd(Do_Intercept)                       0.58      0.09     0.43     0.78 1.00
#> sd(Do_racearabic)                      0.18      0.10     0.01     0.39 1.01
#> sd(g1x_Intercept)                      1.11      0.16     0.83     1.47 1.00
#> sd(g1x_racearabic)                     0.29      0.09     0.13     0.47 1.00
#> sd(g2x_Intercept)                      0.52      0.10     0.33     0.74 1.00
#> sd(g2x_racearabic)                     0.46      0.10     0.27     0.68 1.00
#> cor(Intercept,racearabic)              0.02      0.33    -0.61     0.64 1.00
#> cor(Intercept,Do_Intercept)            0.42      0.18     0.03     0.71 1.01
#> cor(racearabic,Do_Intercept)           0.13      0.34    -0.56     0.71 1.01
#> cor(Intercept,Do_racearabic)          -0.01      0.29    -0.57     0.55 1.00
#> cor(racearabic,Do_racearabic)         -0.06      0.34    -0.66     0.61 1.00
#> cor(Do_Intercept,Do_racearabic)       -0.25      0.30    -0.74     0.41 1.00
#> cor(Intercept,g1x_Intercept)          -0.05      0.21    -0.45     0.37 1.01
#> cor(racearabic,g1x_Intercept)          0.05      0.33    -0.61     0.64 1.01
#> cor(Do_Intercept,g1x_Intercept)        0.02      0.17    -0.31     0.35 1.00
#> cor(Do_racearabic,g1x_Intercept)       0.22      0.28    -0.36     0.70 1.00
#> cor(Intercept,g1x_racearabic)          0.16      0.25    -0.35     0.62 1.00
#> cor(racearabic,g1x_racearabic)        -0.04      0.33    -0.64     0.61 1.01
#> cor(Do_Intercept,g1x_racearabic)      -0.23      0.22    -0.66     0.23 1.00
#> cor(Do_racearabic,g1x_racearabic)     -0.02      0.31    -0.62     0.58 1.00
#> cor(g1x_Intercept,g1x_racearabic)     -0.31      0.25    -0.73     0.23 1.00
#> cor(Intercept,g2x_Intercept)          -0.32      0.22    -0.71     0.12 1.00
#> cor(racearabic,g2x_Intercept)         -0.07      0.34    -0.67     0.60 1.00
#> cor(Do_Intercept,g2x_Intercept)        0.09      0.19    -0.28     0.47 1.00
#> cor(Do_racearabic,g2x_Intercept)      -0.09      0.29    -0.64     0.48 1.00
#> cor(g1x_Intercept,g2x_Intercept)       0.25      0.21    -0.19     0.61 1.01
#> cor(g1x_racearabic,g2x_Intercept)     -0.19      0.25    -0.65     0.33 1.00
#> cor(Intercept,g2x_racearabic)         -0.33      0.21    -0.70     0.11 1.00
#> cor(racearabic,g2x_racearabic)         0.06      0.33    -0.59     0.67 1.00
#> cor(Do_Intercept,g2x_racearabic)      -0.19      0.19    -0.54     0.19 1.00
#> cor(Do_racearabic,g2x_racearabic)      0.13      0.30    -0.49     0.65 1.01
#> cor(g1x_Intercept,g2x_racearabic)     -0.08      0.20    -0.47     0.31 1.00
#> cor(g1x_racearabic,g2x_racearabic)    -0.18      0.27    -0.67     0.35 1.00
#> cor(g2x_Intercept,g2x_racearabic)     -0.14      0.23    -0.55     0.35 1.00
#>                                    Bulk_ESS Tail_ESS
#> sd(Intercept)                           947     1757
#> sd(racearabic)                          702     1478
#> sd(Do_Intercept)                       1678     2638
#> sd(Do_racearabic)                       506     1088
#> sd(g1x_Intercept)                      1485     2445
#> sd(g1x_racearabic)                     1635     1914
#> sd(g2x_Intercept)                       835     1884
#> sd(g2x_racearabic)                     1123     1817
#> cor(Intercept,racearabic)              4433     2951
#> cor(Intercept,Do_Intercept)             661     1333
#> cor(racearabic,Do_Intercept)            270      580
#> cor(Intercept,Do_racearabic)           2667     2774
#> cor(racearabic,Do_racearabic)          1006     1788
#> cor(Do_Intercept,Do_racearabic)        1640     2827
#> cor(Intercept,g1x_Intercept)            338      945
#> cor(racearabic,g1x_Intercept)           169      362
#> cor(Do_Intercept,g1x_Intercept)         897     1456
#> cor(Do_racearabic,g1x_Intercept)        405      741
#> cor(Intercept,g1x_racearabic)          1957     2193
#> cor(racearabic,g1x_racearabic)          463     1243
#> cor(Do_Intercept,g1x_racearabic)       3389     3095
#> cor(Do_racearabic,g1x_racearabic)      1708     2562
#> cor(g1x_Intercept,g1x_racearabic)      2990     3267
#> cor(Intercept,g2x_Intercept)            882     1881
#> cor(racearabic,g2x_Intercept)           337      978
#> cor(Do_Intercept,g2x_Intercept)        1699     2406
#> cor(Do_racearabic,g2x_Intercept)        936     1486
#> cor(g1x_Intercept,g2x_Intercept)        617     1330
#> cor(g1x_racearabic,g2x_Intercept)       991     2689
#> cor(Intercept,g2x_racearabic)          1167     1806
#> cor(racearabic,g2x_racearabic)          592     1116
#> cor(Do_Intercept,g2x_racearabic)       2065     2941
#> cor(Do_racearabic,g2x_racearabic)       569     1376
#> cor(g1x_Intercept,g2x_racearabic)      1608     2711
#> cor(g1x_racearabic,g2x_racearabic)     1669     2546
#> cor(g2x_Intercept,g2x_racearabic)      1344     2507
#> 
#> ~stim (Number of levels: 200) 
#>                                  Estimate Est.Error l-95% CI u-95% CI Rhat
#> sd(Intercept)                        0.87      0.14     0.62     1.18 1.01
#> sd(Do_Intercept)                     0.43      0.06     0.33     0.55 1.00
#> sd(g1x_Intercept)                    0.06      0.04     0.00     0.15 1.01
#> sd(g2x_Intercept)                    0.42      0.06     0.30     0.54 1.01
#> cor(Intercept,Do_Intercept)          0.32      0.17    -0.01     0.63 1.01
#> cor(Intercept,g1x_Intercept)        -0.16      0.40    -0.83     0.69 1.00
#> cor(Do_Intercept,g1x_Intercept)     -0.25      0.39    -0.87     0.59 1.00
#> cor(Intercept,g2x_Intercept)        -0.46      0.21    -0.85    -0.03 1.01
#> cor(Do_Intercept,g2x_Intercept)     -0.05      0.18    -0.38     0.33 1.01
#> cor(g1x_Intercept,g2x_Intercept)    -0.11      0.41    -0.81     0.76 1.04
#>                                  Bulk_ESS Tail_ESS
#> sd(Intercept)                         833     1772
#> sd(Do_Intercept)                      992     2371
#> sd(g1x_Intercept)                     765     1630
#> sd(g2x_Intercept)                     398     1042
#> cor(Intercept,Do_Intercept)           527     1029
#> cor(Intercept,g1x_Intercept)         3292     2264
#> cor(Do_Intercept,g1x_Intercept)      3094     3060
#> cor(Intercept,g2x_Intercept)          336      825
#> cor(Do_Intercept,g2x_Intercept)       475     1041
#> cor(g1x_Intercept,g2x_Intercept)       78       89
#> 
#> Regression Coefficients:
#>                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept         -0.21      0.24    -0.70     0.22 1.01      435     1310
#> Do_Intercept       0.16      0.12    -0.09     0.39 1.00     1165     2211
#> g1x_Intercept     -1.39      0.18    -1.78    -1.05 1.00     1729     2139
#> g2x_Intercept     -0.52      0.14    -0.80    -0.24 1.00      788     1568
#> racearabic        -0.66      0.21    -1.09    -0.25 1.00     1413     1775
#> Do_racearabic     -0.03      0.10    -0.23     0.17 1.00     1779     2604
#> g1x_racearabic     0.10      0.08    -0.06     0.26 1.00     2840     3170
#> g2x_racearabic     0.27      0.14    -0.01     0.53 1.00     1325     2307
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
```

### Data Source

Singmann, H., Kellen, D., & Klauer, K. C. (2013). Investigating the
Other-Race Effect of Germans towards Turks and Arabs using Multinomial
Processing Tree Models. In M. Knauff, M. Pauen, N. Sebanz, & I.
Wachsmuth (Eds.), Proceedings of the 35th Annual Conference of the
Cognitive Science Society (pp. 1330–1335). Austin, TX: Cognitive Science
Society. <http://singmann.org/download/publications/SKK-CogSci2013.pdf>
