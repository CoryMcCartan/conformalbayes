#' Estimate leave-one-out conformal interval widths for a fit model
#'
#' Construct finite-sample calibrated predictive intervals for Bayesian models,
#' following the approach in Barber et al. (2021). These intervals are
#' calculated efficiently using importance sampling for the leave-one-out
#' residuals.  By default, the intervals will also reflect the relative
#' uncertainty in the Bayesian model, using the locally-weighted conformal
#' methods of Lei et al. (2018).
#'
#' @param fit Model fit
#' @param truth True values to predict
#'
#' @returns
#'
#' @examples
#'
#' @references
#' Barber, R. F., Candes, E. J., Ramdas, A., & Tibshirani, R. J. (2021).
#' Predictive inference with the jackknife+. *The Annals of Statistics, 49*(1),
#' 486-507.
#'
#' Lei, J., G’Sell, M., Rinaldo, A., Tibshirani, R. J., & Wasserman, L. (2018).
#' Distribution-free predictive inference for regression. *Journal of the
#' American Statistical Association, 113*(523), 1094-1111.
#'
#' @export loo_conformal
loo_conformal = function(fit, ...) {
    UseMethod("loo_conformal")
}

#' @rdname loo_conformal
#' @export
loo_conformal.default = function(fit, truth, est_fun=c("mean", "median"), local=TRUE, ...) {
}

#' @rdname loo_conformal
#' @export
loo_conformal.stanreg = function(fit, est_fun=c("mean", "median"), local=TRUE, ...) {
}

#' @rdname loo_conformal
#' @export
loo_conformal.brmsfit = function(fit, est_fun=c("mean", "median"), local=TRUE, ...) {
}

#' @rdname loo_conformal
#' @export
loo_conformal.array = function(fit, truth, est_fun=c("mean", "median"), local=TRUE, ...) {
}

fit_conformal = function(truth, estimate, est_fun=matrixStats::colMeans2, local=TRUE) {
}
