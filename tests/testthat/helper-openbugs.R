# Tests that need the OpenBUGS program run only where it is installed: the
# openbugs workflow builds it; the fleet check matrix does not have it.
skip_if_no_openbugs <- function() {
  testthat::skip_if(!nzchar(Sys.which("OpenBUGS")), "OpenBUGS is not on the PATH")
}

fake_chains <- function(n = 100, m = 3, names = c("mu", "tau"), shift = 0, seed = 1) {
  set.seed(seed)
  a <- array(rnorm(n * m * length(names)), c(n, m, length(names)))
  a[, , 1] <- a[, , 1] + rep(seq_len(m) * shift, each = n)
  dimnames(a) <- list(NULL, NULL, names)
  a
}
