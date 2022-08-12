
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **conformalbayes** <a href="https://corymccartan.com/conformalbayes/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/conformalbayes)](https://CRAN.R-project.org/package=conformalbayes)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/CoryMcCartan/conformalbayes/workflows/R-CMD-check/badge.svg)](https://github.com/CoryMcCartan/conformalbayes/actions)
<!-- badges: end -->

**conformalbayes** provides functions to construct finite-sample
calibrated predictive intervals for Bayesian models, following the
approach in [Barber et al. (2021)](https://doi.org/10.1214/20-AOS1965).
These intervals are calculated efficiently using importance sampling for
the leave-one-out residuals. By default, the intervals will also reflect
the relative uncertainty in the Bayesian model, using the
locally-weighted conformal methods of [Lei et
al. (2018)](https://doi.org/10.1080/01621459.2017.1307116).

## Installation

You can install the development version of **conformalbayes** with:

``` r
# install.packages("devtools")
devtools::install_github("CoryMcCartan/conformalbayes")
```

## Example

``` r
library(rstanarm)
library(conformalbayes)
data("Loblolly")

fit_idx = sample(nrow(Loblolly), 50)
d_fit = Loblolly[fit_idx, ]
d_test = Loblolly[-fit_idx, ]

# fit a simple linear regression
m = stan_glm(height ~ sqrt(age), data=d_fit,
    chains=1, control=list(adapt_delta=0.999), refresh=0)

# prepare conformal predictions
m = loo_conformal(m)

# make predictive intervals
pred_ci = predictive_interval(m, newdata=d_test, prob=0.9)
print(head(pred_ci))
#>             5%       95%
#> 1  -0.15888597  5.600095
#> 29 25.43314599 30.988491
#> 57 48.67648127 54.182655
#> 2  -0.09561987  5.447242
#> 30 25.42970114 30.938488
#> 72 58.01173186 63.596592

# are we covering?
mean(pred_ci[, "5%"] <= d_test$height &
         d_test$height <= pred_ci[, "95%"])
#> [1] 0.9117647
```

Read more on the [Getting Started
page](https://corymccartan.com/conformalbayes/articles/conformalbayes.html).

## Citations

Barber, R. F., Candes, E. J., Ramdas, A., & Tibshirani, R. J. (2021).
Predictive inference with the jackknife+. *The Annals of Statistics,
49*(1), 486-507.

Lei, J., G’Sell, M., Rinaldo, A., Tibshirani, R. J., & Wasserman, L.
(2018). Distribution-free predictive inference for regression. *Journal
of the American Statistical Association, 113*(523), 1094-1111.
