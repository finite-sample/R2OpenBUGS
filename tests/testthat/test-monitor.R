test_that("Rhat equals coda's potential scale reduction factor", {
  # Both compute Brooks and Gelman's corrected PSRF from the same within and
  # between chain variances. conv.par() caps the degrees of freedom at 1000,
  # so the identity holds only where the estimated df is below that; n = 100
  # keeps it there.
  a <- fake_chains(n = 100, m = 4, names = c("x", "y"), shift = 0.3)
  mon <- monitor(a, keep.all = TRUE, trans = c("", ""))
  chains <- coda::mcmc.list(lapply(seq_len(4), function(j) coda::mcmc(a[, j, ])))
  psrf <- coda::gelman.diag(chains,
    transform = FALSE, autoburnin = FALSE, multivariate = FALSE
  )$psrf[, "Point est."]
  expect_equal(unname(mon[, "Rhat"]), unname(psrf), tolerance = 1e-12)
  expect_gt(mon["x", "Rhat"], 1.05)
})

test_that("mean, sd and quantiles are those of the pooled draws", {
  a <- fake_chains()
  mon <- monitor(a, keep.all = TRUE, trans = c("", ""))
  pooled <- apply(a, 3, as.vector)
  expect_equal(mon[, "mean"], colMeans(pooled))
  expect_equal(mon[, "sd"], apply(pooled, 2, sd))
  q <- apply(pooled, 2, quantile, probs = c(.025, .25, .5, .75, .975))
  expect_equal(unname(mon[, c("2.5%", "25%", "50%", "75%", "97.5%")]), unname(t(q)))
})

test_that("keep.all = FALSE summarizes only the second half of each chain", {
  a <- fake_chains(n = 200)
  expect_equal(
    monitor(a, keep.all = FALSE, trans = c("", "")),
    monitor(a[101:200, , , drop = FALSE], keep.all = TRUE, trans = c("", ""))
  )
})

test_that("a single chain has no Rhat or n.eff columns", {
  a <- fake_chains(m = 1)
  mon <- monitor(a, n.chains = 1, keep.all = TRUE, trans = c("", ""))
  expect_identical(colnames(mon), c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%"))
})

test_that("a constant parameter is treated as converged", {
  a <- fake_chains()
  a[, , 2] <- 1
  mon <- monitor(a, keep.all = TRUE, trans = c("", ""))
  expect_identical(mon["tau", c("Rhat", "n.eff")], c(Rhat = 1, n.eff = 1))
})
