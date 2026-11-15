"bugs" <-
  function(
    data, inits, parameters.to.save, n.iter, model.file = "model.txt",
    n.chains = 3, n.burnin = floor(n.iter / 2), n.thin = 1,
    saveExec = FALSE, restart = FALSE,
    debug = FALSE, DIC = TRUE, digits = 5, codaPkg = FALSE,
    OpenBUGS.pgm = NULL,
    working.directory = NULL,
    clearWD = FALSE, useWINE = FALSE, WINE = NULL,
    newWINE = TRUE, WINEPATH = NULL, bugs.seed = 1, summary.only = FALSE,
    save.history = (.Platform$OS.type == "windows" | useWINE == TRUE),
    over.relax = FALSE
  ) {
    check_bugs_args(
      parameters.to.save = parameters.to.save, n.iter = n.iter, model.file = model.file,
      n.chains = n.chains, n.burnin = n.burnin, n.thin = n.thin, saveExec = saveExec,
      restart = restart, debug = debug, DIC = DIC, digits = digits, codaPkg = codaPkg,
      working.directory = working.directory, clearWD = clearWD, useWINE = useWINE,
      newWINE = newWINE, bugs.seed = bugs.seed, summary.only = summary.only,
      save.history = save.history, over.relax = over.relax
    )
    if (is.null(OpenBUGS.pgm)) {
      OpenBUGS.pgm <- findOpenBUGS()
      if (.Platform$OS.type == "windows" || useWINE) {
        OpenBUGS.pgm <- file.path(OpenBUGS.pgm, "OpenBUGS.exe")
      }
    } else if (OpenBUGS.pgm == "OpenBUGS") {
      OpenBUGS.pgm <- Sys.which("OpenBUGS")
    }

    if (!file.exists(OpenBUGS.pgm)) {
      cli::cli_abort(c(
        "Cannot find the OpenBUGS program.",
        i = "Set {.arg OpenBUGS.pgm}, or put {.code OpenBUGS} on the PATH."
      ))
    }

    ## Is OpenBUGS.pgm defined in Windows (where second character is :
    ## i.e. C:\Program...) or Unix style path?
    if (useWINE && (substr(OpenBUGS.pgm, 2, 2) == ":")) {
      OpenBUGS.pgm <- win2native(OpenBUGS.pgm, newWINE = newWINE, WINEPATH = WINEPATH)
    }

    ### check options for unix/linux
    if (.Platform$OS.type != "windows" && !useWINE) {
      if (debug) cli::cli_abort("{.arg debug} is not available on Linux or macOS.")
      if (save.history) {
        cli::cli_abort("{.arg save.history} (history plots) is not available on Linux or macOS.")
      }
    }

    if (!is.function(model.file) && length(grep("\\.bug", tolower(model.file)))) {
      cli::cli_abort("{.arg model.file} must end in {.file .txt}, not {.file .bug}.")
    }

    if (is.null(working.directory) && (saveExec || restart)) {
      cli::cli_abort(
        "{.arg working.directory} must be set when {.arg saveExec} or {.arg restart} is TRUE."
      )
    }

    if (!is.null(working.directory)) {
      working.directory <- path.expand(working.directory)
      savedWD <- getwd()
      setwd(working.directory)
      on.exit(setwd(savedWD))
    }

    ## Checking number of inits, which is NOT saved here:
    if (!missing(inits) && !is.function(inits) && !is.null(inits) && (length(inits) != n.chains)) {
      cli::cli_abort(
        "{.arg inits} has {length(inits)} element{?s}, but {.arg n.chains} is {n.chains}."
      )
    }

    ## Wine
    if (useWINE) {
      ## Attempt to find wine and winepath
      if (is.null(WINE)) WINE <- findUnixBinary(x = "wine")
      if (is.null(WINEPATH)) WINEPATH <- findUnixBinary(x = "winepath")
    }

    ## Move to working drirectory or temporary directory when NULL
    inTempDir <- FALSE
    if (is.null(working.directory)) {
      working.directory <- tempdir()
      if (useWINE) {
        ## Some tweaks for wine (particularly required for Mac OS)
        working.directory <- gsub("//", "/", working.directory)
        Sys.chmod(working.directory, mode = "770")
        on.exit(Sys.chmod(working.directory, mode = "700"), add = TRUE)
      }
      savedWD <- getwd()
      setwd(working.directory)
      on.exit(setwd(savedWD), add = TRUE)
      inTempDir <- TRUE
    }

    ## model.file is not a file name but a model function
    if (is.function(model.file)) {
      temp <- tempfile("model")
      temp <- paste(temp, "txt", sep = ".")
      write.model(model.file, con = temp, digits = digits)
      model.file <- gsub("\\\\", "/", temp)
    }
    if (inTempDir && basename(model.file) == model.file) {
      try(file.copy(file.path(savedWD, model.file), model.file, overwrite = TRUE))
    }
    if (!file.exists(model.file)) {
      cli::cli_abort("Model file {.file {model.file}} does not exist.")
    }
    if (file.info(model.file)$isdir) {
      cli::cli_abort("{.arg model.file} {.file {model.file}} is a directory, not a file.")
    }
    data_is_file <- length(data) == 1 && is.vector(data) && is.character(data) &&
      (regexpr("\\.txt$", data) > 0)
    if (!data_is_file) {
      bugs.data.file <- bugs.data(data, dir = getwd(), digits, envir = parent.frame())
    } else {
      if (inTempDir && all(basename(data) == data)) {
        try(file.copy(file.path(savedWD, data), data, overwrite = TRUE))
      }
      if (!file.exists(data)) {
        cli::cli_abort("Data file {.file {data}} does not exist.")
      }
      bugs.data.file <- data
    }

    if (is.character(inits)) {
      if (inTempDir && all(basename(inits) == inits)) {
        try(file.copy(file.path(savedWD, inits), inits, overwrite = TRUE))
      }
      if (!all(file.exists(inits))) {
        cli::cli_abort("Inits file{?s} {.file {inits[!file.exists(inits)]}} {?does/do} not exist.")
      }
      if (length(inits) != n.chains) {
        cli::cli_abort(c(
          "{.arg inits} names {length(inits)} file{?s}, but {.arg n.chains} is {n.chains}.",
          i = "Give one inits file per chain."
        ))
      }
      bugs.inits.files <- inits
    } else {
      if (!is.function(inits) && !is.null(inits) && (length(inits) != n.chains)) {
        cli::cli_abort(
          "{.arg inits} has {length(inits)} element{?s}, but {.arg n.chains} is {n.chains}."
        )
      }
      bugs.inits.files <- bugs.inits(inits, n.chains, digits)
    }

    if (DIC) parameters.to.save <- c(parameters.to.save, "deviance")
    ## Model files must have extension ".txt"
    if (!length(grep("\\.txt$", tolower(model.file)))) {
      new.model.file <- paste(basename(model.file), ".txt", sep = "")
      if (!is.null(working.directory)) {
        new.model.file <- file.path(working.directory, new.model.file)
      }
      file.copy(model.file, new.model.file, overwrite = TRUE)
      on.exit(try(file.remove(new.model.file)), add = TRUE)
    } else {
      new.model.file <- model.file
    }

    ## Create a filename model.file + .bug
    model.file.bug <- gsub("\\.txt", ".bug", basename(new.model.file))

    if (restart && !file.exists(model.file.bug)) {
      cli::cli_abort(
        "Restart file {.file {model.file.bug}} was not found in the working directory."
      )
    }

    if (useWINE) {
      ## Some tweaks for wine (particularly required for Mac OS)
      new.model.file <- gsub("//", "/", new.model.file)
    }
    bugs.script(parameters.to.save, n.chains, n.iter, n.burnin, n.thin,
      saveExec, restart, model.file.bug,
      new.model.file,
      debug = debug, is.inits = !is.null(inits),
      DIC = DIC, useWINE = useWINE, newWINE = newWINE,
      WINEPATH = WINEPATH, bugs.seed = bugs.seed,
      summary.only = summary.only, save.history = save.history,
      bugs.data.file = bugs.data.file,
      bugs.inits.files = bugs.inits.files, over.relax = over.relax
    )
    bugs.run(n.burnin, OpenBUGS.pgm,
      debug = debug, WINE = WINE, useWINE = useWINE,
      newWINE = newWINE, WINEPATH = WINEPATH
    )
    if (codaPkg) {
      return(file.path(getwd(), paste("CODAchain", 1:n.chains, ".txt", sep = "")))
    }
    if (summary.only) {
      return(bugs.log("log.txt"))
    }

    sims <- c(
      bugs.sims(
        parameters.to.save, n.chains, n.iter, n.burnin,
        n.thin, DIC
      ),
      model.file = model.file
    )
    if (clearWD) {
      file.remove(c(
        bugs.data.file, "log.odc", "log.txt", "CODAIndex.txt",
        bugs.inits.files, "script.txt",
        paste("CODAchain", 1:n.chains, ".txt", sep = "")
      ))
    }
    class(sims) <- "bugs"
    sims
  }
