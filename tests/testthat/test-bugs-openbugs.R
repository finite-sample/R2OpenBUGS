schools_fit <- function(seed = 1) {
  wd <- withr::local_tempdir(.local_envir = parent.frame())
  model.file <- system.file("model", "schools.txt", package = "R2OpenBUGS")
  data_env <- new.env()
  utils::data("schools", package = "R2OpenBUGS", envir = data_env)
  schools <- data_env$schools
  J <- nrow(schools)
  y <- schools$estimate
  sigma.y <- schools$sd
  set.seed(seed)
  inits <- function() {
    list(theta = rnorm(J, 0, 100), mu.theta = rnorm(1, 0, 100), sigma.theta = runif(1, 0, 100))
  }
  bugs(list(J = J, y = y, sigma.y = sigma.y), inits,
    c("theta", "mu.theta", "sigma.theta"), model.file,
    n.chains = 3, n.iter = 2000, working.directory = wd
  )
}

test_that("bugs() summary agrees with its own draws and with coda", {
  skip_if_no_openbugs()
  fit <- schools_fit()
  expect_s3_class(fit, "bugs")
  expect_identical(dim(fit$sims.array), c(1000L, 3L, 11L))
  expect_equal(fit$summary[, "mean"], colMeans(fit$sims.matrix))
  expect_equal(as.vector(fit$mean$theta), unname(colMeans(fit$sims.list$theta)))
  expect_equal(fit$summary, monitor(fit$sims.array, keep.all = TRUE))
  chains <- as.mcmc.list(fit)
  expect_equal(summary(chains)$statistics[, "Mean"], fit$summary[colnames(chains[[1]]), "mean"])
})

test_that("last.values are each chain's final draw, usable as inits", {
  skip_if_no_openbugs()
  fit <- schools_fit()
  for (i in 1:3) {
    last_theta <- fit$sims.array[1000, i, paste0("theta[", 1:8, "]")]
    expect_equal(fit$last.values[[i]]$theta, unname(last_theta))
    expect_equal(fit$last.values[[i]]$mu.theta, unname(fit$sims.array[1000, i, "mu.theta"]))
  }
})

test_that("the schools posterior matches the known answer", {
  skip_if_no_openbugs()
  fit <- schools_fit()
  # Gelman et al., Bayesian Data Analysis, sec. 5.5: posterior mean of the
  # population effect near 8, between-school sd near 6-7.
  expect_lt(abs(fit$mean$mu.theta - 8), 2)
  expect_true(fit$mean$sigma.theta > 4 && fit$mean$sigma.theta < 9)
  expect_true(all(fit$summary[, "Rhat"] < 1.1))
  expect_true(is.finite(fit$pD) && is.finite(fit$DIC))
})

test_that("n.thin thins inside OpenBUGS: n.keep stays n.iter - n.burnin", {
  skip_if_no_openbugs()
  model <- withr::local_tempfile(fileext = ".txt")
  writeLines("model { mu ~ dnorm(0, 1) }", model)
  fit <- bugs(list(y = 1), NULL, "mu", model,
    n.chains = 2, n.iter = 300, n.burnin = 100, n.thin = 3, DIC = FALSE,
    working.directory = withr::local_tempdir()
  )
  expect_identical(fit$n.keep, 200)
  expect_identical(dim(fit$sims.array)[1], 200L)
})
