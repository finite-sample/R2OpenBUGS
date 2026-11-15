# Every out-of-domain value must fail in bugs() itself, naming the argument,
# before OpenBUGS is looked for -- these run without OpenBUGS installed.
call_bugs <- function(...) {
  args <- utils::modifyList(
    list(
      data = list(y = 1), inits = NULL, parameters.to.save = "mu", n.iter = 100,
      model.file = "model.txt"
    ),
    list(...)
  )
  do.call(bugs, args)
}

test_that("counts outside their domain are rejected with the argument's name", {
  bad <- list(
    n.iter = list(0, -5, 10.5, NA, "100", c(100, 200)),
    n.chains = list(0, 1.5, NA),
    n.burnin = list(-1, 2.5),
    n.thin = list(0, -1, 1.5),
    digits = list(0, 2.5)
  )
  for (arg in names(bad)) {
    for (value in bad[[arg]]) {
      expect_error(do.call(call_bugs, stats::setNames(list(value), arg)), arg, fixed = TRUE)
    }
  }
})

test_that("n.iter must leave at least two kept iterations", {
  expect_error(call_bugs(n.iter = 100, n.burnin = 99), "must exceed")
  expect_error(call_bugs(n.iter = 100, n.burnin = 100), "must exceed")
  expect_error(call_bugs(n.iter = 1), "must exceed")
})

test_that("bugs.seed must be one of OpenBUGS's 14 seed states", {
  for (value in list(0, 15, 2.5, "3")) {
    expect_error(call_bugs(bugs.seed = value), "bugs.seed", fixed = TRUE)
  }
})

test_that("flags must be single TRUE or FALSE", {
  for (arg in c("DIC", "codaPkg", "summary.only", "over.relax", "clearWD", "restart")) {
    for (value in list(NA, "yes", c(TRUE, FALSE), 1)) {
      expect_error(do.call(call_bugs, stats::setNames(list(value), arg)), arg, fixed = TRUE)
    }
  }
})

test_that("parameters.to.save, model.file and working.directory are checked", {
  expect_error(call_bugs(parameters.to.save = character(0)), "parameters.to.save", fixed = TRUE)
  expect_error(call_bugs(parameters.to.save = c("mu", NA)), "parameters.to.save", fixed = TRUE)
  expect_error(call_bugs(model.file = 3), "model.file", fixed = TRUE)
  expect_error(
    call_bugs(working.directory = file.path(tempdir(), "no-such-dir")),
    "working.directory",
    fixed = TRUE
  )
})

test_that("valid arguments get past the checks to the OpenBUGS lookup", {
  skip_if(nzchar(Sys.which("OpenBUGS")), "OpenBUGS is installed here")
  withr::local_envvar(OpenBUGS_PATH = "")
  withr::local_options(R2OpenBUGS.pgm = NULL)
  expect_error(call_bugs(), "Cannot find the OpenBUGS program")
})

test_that("debug and save.history are refused off Windows instead of ignored", {
  skip_on_os("windows")
  fake <- withr::local_tempfile()
  file.create(fake)
  expect_error(call_bugs(OpenBUGS.pgm = fake, debug = TRUE), "debug", fixed = TRUE)
  expect_error(call_bugs(OpenBUGS.pgm = fake, save.history = TRUE), "save.history", fixed = TRUE)
})
