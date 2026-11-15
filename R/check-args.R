# Argument checks for bugs(), run before OpenBUGS is looked for, so a bad
# value fails here with the argument's name rather than inside OpenBUGS.
check_bugs_args <- function(parameters.to.save, n.iter, model.file, n.chains, n.burnin,
                            n.thin, saveExec, restart, debug, DIC, digits, codaPkg,
                            working.directory, clearWD, useWINE, newWINE, bugs.seed,
                            summary.only, save.history, over.relax) {
  checkmate::assert_character(parameters.to.save, min.chars = 1, any.missing = FALSE, min.len = 1)
  checkmate::assert_count(n.iter, positive = TRUE)
  checkmate::assert_count(n.chains, positive = TRUE)
  checkmate::assert_count(n.burnin)
  checkmate::assert_count(n.thin, positive = TRUE)
  checkmate::assert_count(digits, positive = TRUE)
  # OpenBUGS needs at least two kept iterations to report a summary.
  if (n.iter - n.burnin < 2) {
    cli::cli_abort(c(
      "{.arg n.iter} must exceed {.arg n.burnin} by at least 2.",
      x = "{.arg n.iter} is {n.iter} and {.arg n.burnin} is {n.burnin}."
    ), call = rlang::caller_env())
  }
  checkmate::assert(
    checkmate::check_function(model.file),
    checkmate::check_string(model.file, min.chars = 1),
    .var.name = "model.file"
  )
  checkmate::assert_string(working.directory, min.chars = 1, null.ok = TRUE)
  if (!is.null(working.directory)) {
    checkmate::assert_directory_exists(path.expand(working.directory),
      .var.name = "working.directory"
    )
  }
  # OpenBUGS's modelSetRN() takes one of 14 predefined seed states.
  checkmate::assert_int(bugs.seed, lower = 1, upper = 14, null.ok = TRUE)
  flags <- list(
    saveExec = saveExec, restart = restart, debug = debug, DIC = DIC, codaPkg = codaPkg,
    clearWD = clearWD, useWINE = useWINE, newWINE = newWINE, summary.only = summary.only,
    save.history = save.history, over.relax = over.relax
  )
  for (name in names(flags)) checkmate::assert_flag(flags[[name]], .var.name = name)
  invisible(TRUE)
}
