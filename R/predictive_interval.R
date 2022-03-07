#' Jackknife(+) predictive intervals
#'
#' Construct finite-sample calibrated predictive intervals for Bayesian models,
#' following the approach in Barber et al. (2021). By default, the intervals will also reflect the
#' relative uncertainty in the Bayesian model, using the locally-weighted
#' conformal methods of Lei et al. (2018).
#'
#' @param object A fitted model which has been passed through [loo_conformal()]
#' @param probs The coverage probabilities to calculate intervals for.
#'   Empirically, the coverage rate of the constructed intervals will generally
#'   match these probabilities, but the theoretical guarantee for a probability
#'   of \eqn{1-\alpha} is only for coverage of at least \eqn{1-2\alpha}, and
#'   only if `plus=TRUE` (below).
#' @param plus If `TRUE`, construct jackknife+ intervals, which have a
#'   theoretical guarantee. These require higher computational costs, which
#'   scale with both the number of training and prediction points. Defaults to
#'   `TRUE` when both of these numbers are less than 500.
#' @param local If `TRUE` (the default), perform locally-weighted conformal
#'   inference. This will inflate the width of the predictive intervals by a
#'   constant amount across all predictions, preserving the relative amount of
#'   uncertainty captured by the model. If `FALSE`, all predictive intervals
#'   will have (nearly) the same width.
#' @param ... Further arguments to the [posterior_predict()] method for `object`.
#'
#' @returns A matrix with the number of rows matching the number of predictions.
#'   Columns will be labeled with a percentile corresponding to `probs`; e.g. if
#'   `probs=0.9` the columns will be `5%` and `95%`.
#'
#' @examples
#' if (requireNamespace("rstanarm", quietly=TRUE)) suppressWarnings({
#'     library(rstanarm)
#'     # fit a simple linear regression
#'     m = stan_glm(mpg ~ disp + cyl, data=mtcars,
#'         chains=1, iter=1000,
#'         control=list(adapt_delta=0.999), refresh=0)
#'
#'     m = loo_conformal(m)
#'     # make predictive intervals
#'     predictive_interval(m)
#' })
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
#' @method predictive_interval conformal
#' @export
predictive_interval.conformal = function(object, probs=0.9, plus=NULL, local=TRUE, ...) {
    if (any(probs <= 0 | probs >= 1))
        cli_abort("{.arg prob} must greater than 0 and less than 1.")

    preds = rstantools::posterior_predict(object, ...)
    N_new = ncol(preds)

    n_prob = length(probs)
    probs = sort(probs)
    alphas = 0.5 - 0.5*probs
    out = matrix(nrow=N_new, ncol=2*n_prob)
    colnames(out) = paste0(100*c(rev(alphas), 1 - alphas), "%")
    rownames(out) = colnames(preds)

    l = attr(object, ".conformal")
    N = ncol(l$w)
    est_fun = get_est_fun(l$est_fun)
    idx_lo = seq_len(n_prob)
    idx_hi = n_prob + seq_len(n_prob)

    # local sd estimates
    if (isTRUE(local)) {
        loc_sd = matrixStats::colSds(preds)
    } else {
        loc_sd = rep(1, N_new)
        l$sd = 1
    }

    if (is.null(plus))
        plus = N < 500 && N_new < 500

    if (isTRUE(plus)) {
        for (i in seq_len(N_new)) {
            loo_preds = wapply2(preds[, i], l$w, N, est_fun)
            out[i, idx_lo] = quantile(loo_preds - loc_sd[i] * l$resid / l$sd,
                                      probs=rev(1-probs), names=FALSE)
            out[i, idx_hi] = quantile(loo_preds + loc_sd[i] * l$resid / l$sd,
                                      probs=probs, names=FALSE)
        }
    } else {
        if (l$est_fun == "mean") {
            ests = colMeans2(preds)
        } else {
            ests = colMedians(preds)
        }

        roo_q = quantile(l$resid / l$sd, probs=probs, names=FALSE)
        for (i in seq_len(n_prob)) {
            out[, idx_lo[i]] = ests - roo_q[i] * loc_sd
            out[, idx_hi[i]] = ests + roo_q[i] * loc_sd
        }
    }

    out
}
