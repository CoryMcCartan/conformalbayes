skip_on_cran()

library(rstantools)

sim_data = function(n) {
    rcauchy(n)
}

fit_norm = function(y, draws=100) {
    y_mean = mean(y)
    y_sd = sd(y)
    n = length(y)

    sigma = y_sd * sqrt(n - 1) / sqrt(rchisq(draws, n - 1))
    mu = rnorm(draws, y_mean, sigma / sqrt(n))

    structure(list(mu = mu, sigma=sigma, y = y), class="normfit")
}

posterior_predict.normfit = function(object, newdata=NULL, ...) {
    if (is.null(newdata)) newdata = object$y

    n = length(newdata)
    draws = length(object$mu)

    out = rnorm(draws*n, object$mu, object$sigma)
    matrix(out, nrow=draws, ncol=n)
}

log_lik.normfit = function(object, ...) {
    draws = length(object$mu)
    n = length(object$y)
    out = matrix(nrow=draws, ncol=n)
    for (i in seq_len(n)) {
        out[, i] = dnorm(object$y[i], object$mu, object$sigma, log=TRUE)
    }
    out
}

# Register
.S3method("posterior_predict", "normfit")
.S3method("log_lik", "normfit")


test_that("(non-)local jackknife(+) intervals are correct", {
    set.seed(5118)
    coverage = t(vapply(seq_len(500), function(i) {
        y_fit = sim_data(10)
        y_test = sim_data(10)

        m = fit_norm(y_fit)

        pred0 = posterior_predict.normfit(m, newdata=y_test)
        interv0 = matrixStats::colQuantiles(pred0, probs=c(0.25, 0.75))
        cover0 = mean(interv0[,1] <= y_test & y_test <= interv0[,2])

        out = c(cover0, 0, 0, 0, 0)

        m = suppressWarnings(loo_conformal(m))

        interv1 = predictive_interval(m, newdata=y_test, prob=0.5,
                                      plus=TRUE, local=FALSE)
        out[2] = mean(interv1[,1] <= y_test & y_test <= interv1[,2])

        interv1 = predictive_interval(m, newdata=y_test, prob=0.5,
                                      plus=TRUE, local=TRUE)
        out[3] = mean(interv1[,1] <= y_test & y_test <= interv1[,2])

        interv1 = predictive_interval(m, newdata=y_test, prob=0.5,
                                      plus=FALSE, local=FALSE)
        out[4] = mean(interv1[,1] <= y_test & y_test <= interv1[,2])

        interv1 = predictive_interval(m, newdata=y_test, prob=0.5,
                                      plus=FALSE, local=TRUE)
        out[5] = mean(interv1[,1] <= y_test & y_test <= interv1[,2])

        out
    }, numeric(5)))

    p_diff = t.test(coverage[, 1], coverage[, 2])$p.value
    p_cover_plus_global = t.test(coverage[, 2] - 0.5)$p.value
    p_cover_plus_local = t.test(coverage[, 3] - 0.472)$p.value
    p_cover_jack_global = t.test(coverage[, 4] - 0.535)$p.value
    p_cover_jack_local = t.test(coverage[, 5] - 0.501)$p.value

    avg_cov = colMeans(coverage)

    expect_lt(p_diff, 0.01)
    expect_gt(p_cover_plus_global, 0.01)
    expect_gt(p_cover_plus_local, 0.01)
    expect_gt(p_cover_jack_global, 0.01)
    expect_gt(p_cover_jack_local, 0.01)
    expect_true(all(0.45 < avg_cov[-1] & avg_cov[-1] < 0.55))
})
