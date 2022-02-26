#' Enable jackknife+ predictive intervals for a fit model
#'
#' Prepares for jackknife+ conformal prediction by performing Pareto-smoothed
#' importance sampling to yield leave-one-out residuals.
#'
#' @param fit Model fit; an object with `posterior_predict()` and `log_lik()`
#'   methods. Can also be an `array` of posterior predictions.
#' @param truth True values to predict. Not required for `rstanarm` or `brms`
#'   models.
#' @param est_fun Whether to use the posterior `mean` (the default) or `median`
#'   as a point estimate.
#' @param log_lik If `fit` is an array, then `log_lik` should be an array of
#'   matching dimension giving the pointwise log-likelihood.
#' @param chain An integer vector identifying the chain numbers for the
#'   posterior draws. Should be provided if multiple chains are used.
#' @param ... Ignored.
#'
#' @returns A modified `fit` object with an additional class `conformal`.
#'   Calling [predictive_interval()] on this new object will yield conformal
#'   intervals.
#'
#' @examples
#'
#' @references
#' Vehtari, A., Simpson, D., Gelman, A., Yao, Y., & Gabry, J. (2015).
#' Pareto smoothed importance sampling. [arXiv preprint
#' arXiv:1507.02646](https://arxiv.org/abs/1507.02646).
#'
#' @export
loo_conformal = function(fit, ...) {
    UseMethod("loo_conformal")
}

#' @rdname loo_conformal
#' @export
loo_conformal.default = function(fit, truth, chain=NULL,
                                 est_fun=c("mean", "median"), ...) {
    if (!has_generic(fit, "posterior_predict"))
        cli_abort("{.arg fit} must have a {.fn posterior_predict} method.")
    preds = rstantools::posterior_predict(fit)

    if (!has_generic(fit, "log_lik"))
        cli_abort("{.arg fit} must have a {.fn log_lik} method.")
    log_lik = rstantools::log_lik(fit)

    if (missing(truth)) {
        if (!is.null(fit[["y"]])) {
            truth = fit$y
        } else if (has_generic(fit, "fitted") && has_generic(fit, "residuals")) {
            truth = fitted(fit) + residuals(fit)
        } else {
            cli_abort("{.arg fit} must have true values stored under {.arg y},
                      or must have {.fn fitted} and {.fn residuals} methods.")
        }
    }
    if (ncol(preds) != length(truth))
        cli_abort("{.arg truth} doesn't have the same number of data points as {.arg fit}.")

    if (is.null(chain))
        chain = rep(1L, dim(preds)[1])

    make_conformal(fit,
                   truth=truth,
                   preds=preds,
                   log_lik=log_lik,
                   chain=chain,
                   est_fun=match.arg(est_fun))
}

#' @rdname loo_conformal
#' @export
loo_conformal.stanreg = function(fit, est_fun=c("mean", "median"), ...) {
    make_conformal(fit,
                   truth=fit$fitted.values + fit$residuals,
                   preds=rstantools::posterior_predict(fit),
                   log_lik=rstantools::log_lik(fit),
                   chain=get_chain_id.stanreg(fit),
                   est_fun=match.arg(est_fun))
}

#' @rdname loo_conformal
#' @export
loo_conformal.brmsfit = function(fit, est_fun=c("mean", "median"), ...) {
    if (requireNamespace("brms", quietly=TRUE)) {
        truth = brms::get_y(fit)
    } else {
        cli_abort("The {.pkg brms} package must be installed.")
    }

    make_conformal(fit,
                   truth=truth,
                   preds=rstantools::posterior_predict(fit),
                   log_lik=rstantools::log_lik(fit),
                   chain=get_chain_id.brmsfit(fit),
                   est_fun=match.arg(est_fun))
}

#' @rdname loo_conformal
#' @export
loo_conformal.array = function(fit, log_lik, truth, chain=NULL,
                               est_fun=c("mean", "median"), ...) {
    dims = dim(fit)

    if (length(dims) == 2) {
        if (is.null(chain))
            chain = rep(1L, dims[1])
    } else if (length(dims) == 3) {
        n_chains = dims[2]
        extractor = function(j, x) x[, j, ]

        fit = do.call(rbind, lapply(seq_len(n_chains), extractor, x=fit))
        log_lik = do.call(rbind, lapply(seq_len(n_chains), extractor, x=log_lik))

        if (!is.null(chain))
            cli_warn("{.arg chain} is ignored if the input array has more than two dimensions.")
        chain = rep(seq_len(n_chains), each = dims[1])
    } else {
        cli_abort("{.arg fit} must be a 2- or 3-dimensional array.")
    }

    if (any(dim(fit) != dim(log_lik)))
        cli_abort("{.arg fit} and {.arg log_lik} must have matching dimensions.")
    if (ncol(fit) != length(truth))
        cli_abort("{.arg truth} doesn't have the same number of data points as
                  {.arg fit} and {.arg log_lik}.")


    make_conformal(list(),
                   truth=truth, preds=fit, log_lik=log_lik, chain=chain,
                   est_fun=match.arg(est_fun))
}

# Wrap `fit` so that it can do conformal intervals
make_conformal = function(fit, truth, preds, log_lik, chain, est_fun="mean") {
    N = length(truth)
    iter = nrow(log_lik)

    # get the importance sampling weights
    r_eff = loo::relative_eff(exp(log_lik), chain_id=chain)
    psis_obj = loo::psis(-log_lik, r_eff=r_eff)
    w = loo::weights.importance_sampling(psis_obj, log=FALSE, normalize=TRUE)

    loo_ests = colapply2(preds, w, N, get_est_fun(est_fun))
    loo_resid = abs(truth - loo_ests)
    # local CI width estimator
    loo_sd = colapply2(preds, w*iter, N, matrixStats::weightedSd)

    # in-sample difference between prediction and point estimatec
    if (est_fun == "mean") {
        pred_diff = sweep(preds, 2, colMeans2(preds), check.margin=FALSE)
    } else if (est_fun == "median") {
        pred_diff = sweep(preds, 2, colMedians(preds), check.margin=FALSE)
    }

    out = list(
        est_fun = est_fun,
        w = w,
        ests = loo_ests,
        resid = loo_resid,
        sd = loo_sd,
        scale_infl = mean(loo_resid) / mean(abs(pred_diff))
    )

    attr(fit, ".conformal") = out
    class(fit) = c("conformal", class(fit))
    fit
}

#' @method print conformal
#' @export
print.conformal = function(x, ...) {
    NextMethod()
    cli::cli_text("({.pkg conformalbayes} enabled, with estimated CI inflation
                  factor {round(attr(x, '.conformal')$scale_infl, 2)})")
}
