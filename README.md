
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

See `?mpt`

``` r
library(mptstan)
#> Loading required package: brms
#> Loading required package: Rcpp
#> Loading 'brms' package (version 2.21.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> The following object is masked from 'package:stats':
#> 
#>     ar
## basic example code
```
