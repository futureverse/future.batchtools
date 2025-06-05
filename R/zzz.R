## To be cached by .onLoad()
.onLoad <- function(libname, pkgname) {
  import_future_functions()
  
  debug <- getOption("future.debug", FALSE)
  
  inRCmdCheck <- import_parallelly("inRCmdCheck")
  if (inRCmdCheck()) {
    ## Don't write to current working directory when running R CMD check.
    path <- Sys.getenv("R_FUTURE_CACHE_PATH", NA_character_)
    if (is.na(path)) {
      Sys.setenv("R_FUTURE_CACHE_PATH" = file.path(tempdir(), ".future"))
      if (debug) {
        mdebugf("R CMD check detected: Set R_FUTURE_CACHE_PATH=%s",
                sQuote(Sys.getenv("R_FUTURE_CACHE_PATH")))
      }
    }
  }

  update_package_options(debug = debug)
}


#' @importFrom utils file_test
.onUnload <- function(libpath) {
  ## (a) Force finalizer of Future objects to run such
  ##     that their batchtools directories are removed
  gc()

  ## (b) Remove batchtools root directory if only a set
  ##     of known files exists, i.e. not any directories etc.
  path <- future_cache_path(create = FALSE)
  ## Only known files left?
  files <- dir(path = path)
  known_files <- c("sessioninfo.txt")
  if (all(files %in% known_files)) {
    for (file in known_files) {
      pathname_tmp <- file.path(path, file)
      if (file_test("-f", pathname_tmp)) try(file.remove(pathname_tmp))
    }
    try(unlink(path, recursive = FALSE, force = TRUE), silent = TRUE)
  }
}
