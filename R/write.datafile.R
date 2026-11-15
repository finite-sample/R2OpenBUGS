"write.datafile" <-
  function(datalist, towhere, fill = TRUE) {
    if (!is.list(datalist) || is.data.frame(datalist)) {
      cli::cli_abort("{.arg datalist} must be a list, not {.obj_type_friendly {datalist}}.")
    }
    cat(formatdata(datalist), file = towhere, fill = fill)
  }
