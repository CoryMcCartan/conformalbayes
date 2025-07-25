% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loo_conformal.R
\name{loo_conformal}
\alias{loo_conformal}
\alias{loo_conformal.default}
\alias{loo_conformal.stanreg}
\alias{loo_conformal.brmsfit}
\title{Enable leave-one-out conformal predictive intervals for a fit model}
\usage{
loo_conformal(fit, ...)

\method{loo_conformal}{default}(
  fit,
  truth,
  chain = NULL,
  trans = function(x) x,
  inv_trans = function(x) x,
  est_fun = c("mean", "median"),
  ...
)

\method{loo_conformal}{stanreg}(
  fit,
  trans = function(x) x,
  inv_trans = function(x) x,
  est_fun = c("mean", "median"),
  ...
)

\method{loo_conformal}{brmsfit}(
  fit,
  trans = function(x) x,
  inv_trans = function(x) x,
  est_fun = c("mean", "median"),
  ...
)
}
\arguments{
\item{fit}{Model fit; an object with \code{posterior_predict()} and \code{log_lik()}
methods. Can also be an \code{array} of posterior predictions.}

\item{...}{Ignored.}

\item{truth}{True values to predict. Not required for \code{rstanarm} or \code{brms}
models.}

\item{chain}{An integer vector identifying the chain numbers for the
posterior draws. Should be provided if multiple chains are used.}

\item{trans, inv_trans}{A pair of functions to transform the predictions
before performing conformal inference.}

\item{est_fun}{Whether to use the posterior \code{mean} (the default) or \code{median}
as a point estimate.}
}
\value{
A modified \code{fit} object with an additional class \code{conformal}.
Calling \link[rstantools:predictive_interval]{predictive_interval()} on this
new object will yield conformal intervals.
}
\description{
Prepares for jackknife(+) conformal prediction by performing Pareto-smoothed
importance sampling to yield leave-one-out residuals.
}
\examples{
if (requireNamespace("rstanarm", quietly=TRUE)) suppressWarnings({
    library(rstanarm)
    # fit a simple linear regression
    m = stan_glm(mpg ~ disp + cyl, data=mtcars,
        chains=1, iter=500,
        control=list(adapt_delta=0.999), refresh=0)

    loo_conformal(m)
})

}
\references{
Vehtari, A., Simpson, D., Gelman, A., Yao, Y., & Gabry, J. (2015).
Pareto smoothed importance sampling. \href{https://arxiv.org/abs/1507.02646}{arXiv preprint arXiv:1507.02646}.
}
