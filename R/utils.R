# check for generic
has_generic = function(obj, generic) {
    checker = function(cl) !is.null(utils::getS3method(generic, cl, optional=TRUE))
    any(vapply(class(obj), checker, logical(1)))
}

# re-used a couple times
get_est_fun = function(est_fun="mean") {
    list(mean=matrixStats::weightedMean,
         median=matrixStats::weightedMedian)[[est_fun]]
}

# apply to columns over 2 matrices
colapply2 = function(x1, x2,  n=ncol(x1), fn) {
    out = numeric(n)
    for (i in seq_len(n)) {
        out[i] = fn(x1[, i], x2[, i])
    }
    out
}
# apply to columns over 1 vector and 1 matrix
wapply2 = function(x1, x2, n=ncol(x2), fn) {
    out = numeric(n)
    for (i in seq_len(n)) {
        out[i] = fn(x1, x2[, i])
    }
    out
}


# Get chain IDs for relative eff calculations
get_chain_id.stanreg = function(fit) {
    iter = rstantools::nsamples(fit)
    n_chains = fit$stanfit@sim$chains
    rep(seq_len(n_chains), each=iter/n_chains)
}
get_chain_id.brmsfit = function(fit) {
    iter = rstantools::nsamples(fit)
    n_chains = fit$fit@sim$chains
    rep(seq_len(n_chains), each=iter/n_chains)
}
