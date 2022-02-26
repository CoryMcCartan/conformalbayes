
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **conformalbayes** <a href="https://corymccartan.github.io/conformalbayes/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

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
the leave-out-one residuals. By default, the intervals will also reflect
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

## Citations

Barber, R. F., Candes, E. J., Ramdas, A., & Tibshirani, R. J. (2021).
Predictive inference with the jackknife+. *The Annals of Statistics,
49*(1), 486-507.

Lei, J., G’Sell, M., Rinaldo, A., Tibshirani, R. J., & Wasserman, L.
(2018). Distribution-free predictive inference for regression. *Journal
of the American Statistical Association, 113*(523), 1094-1111.