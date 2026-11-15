"bugs.run" <-
  function(n.burnin, OpenBUGS.pgm, debug = FALSE,
           useWINE = FALSE, WINE = NULL,
           newWINE = TRUE, WINEPATH = NULL) {
    ## Call Bugs and have it run with script.txt
    if (.Platform$OS.type == "windows" || useWINE) {
      bugsCall <- paste("\"", OpenBUGS.pgm, "\" /PAR \"",
        native2win(file.path(getwd(), "script.txt"),
          useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH
        ),
        "\" /",
        sep = ""
      )
      if (!debug) bugsCall <- paste(bugsCall, "HEADLESS", sep = "")
      if (useWINE) bugsCall <- paste(WINE, bugsCall)
    } else {
      bugsCall <- paste(OpenBUGS.pgm, "<", "script.txt", ">", file.path(getwd(), "log.txt"))
    }
    if ((.Platform$OS.type == "windows" || useWINE) && debug) {
      temp <- system(bugsCall, invisible = FALSE)
    } else {
      temp <- system(bugsCall)
    }

    if (temp == -1) {
      stop("Error in bugs.run().")
    }

    ##   !!!! need to check these conditions
    ## Stop and print an error message if Bugs did not run correctly
    tmp <- scan("CODAchain1.txt", character(), quiet = TRUE, sep = "\n")
    tmp <- tmp[seq_len(min(100, length(tmp)))]
    if (length(grep("OpenBUGS did not run correctly", tmp)) > 0) {
      stop(paste(
        "Look at the log file in ", getwd(),
        " and\ntry again with 'debug=TRUE' to figure out what went wrong within OpenBUGS."
      ))
    }
  }
