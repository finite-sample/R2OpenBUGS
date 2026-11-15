test_that("matrices are written in the row-major order BUGS reads", {
  dir <- withr::local_tempdir()
  m <- matrix(1:6, 2, 3)
  bugs.data(list(m = m), dir = dir, digits = 2)
  txt <- paste(readLines(file.path(dir, "data.txt")), collapse = "")
  values <- as.numeric(regmatches(txt, gregexpr("[0-9.]+E[+-][0-9]+", txt))[[1]])
  expect_identical(values, as.numeric(t(m)))
  expect_match(txt, ".Dim=c(2, 3)", fixed = TRUE)
})

test_that("write.model writes small and large constants in E notation", {
  path <- withr::local_tempfile(fileext = ".txt")
  model <- function() {
    for (i in 1:8) {
      y[i] ~ dnorm(mu, 0.0001)
    }
    mu ~ dnorm(0, 1.0E-6)
  }
  write.model(model, path)
  txt <- readLines(path)
  expect_identical(txt[1], "model")
  expect_true(any(grepl("dnorm(mu, 1.00000E-04)", txt, fixed = TRUE)))
  expect_true(any(grepl("dnorm(0.00000E+00, 1.00000E-06)", txt, fixed = TRUE)))
})
