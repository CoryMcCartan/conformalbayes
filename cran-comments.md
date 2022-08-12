## Test environments

* local R installation (macOS), R 4.2.0
* macos-latest (on GitHub Actions), (release)
* windows-latest (on GitHub Actions), (release)
* ubuntu-latest (on GitHub Actions), (release)
* ubuntu-latest (on GitHub Actions), (old release)
* ubuntu-latest (on GitHub Actions), (devel)


## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a resubmission.  It fixes a URL in the DESCRIPTION which had been
redirected. This release also fixes CRAN notes caused by documentation
incompatible with HTML5 standards.

* Examples should take less than five seconds to run but may occasionally exceed
that time depending on the computing power of the environment in which they are
run. Other than making all the function examples \dontrun{}, there are no
possible simpler examples that would execute faster (since each example requires
fitting a Bayesian model with MCMC).

* Tests are skipped on CRAN because they are probabilistic correctness tests and
may take a long time to run. They are run locally.
