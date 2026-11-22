is_na <- function(x) {
  if (length(x) != 1L) return(FALSE)
  is.na(x)
}

attached_packages <- function() {
  pkgs <- search()
  pkgs <- grep("^package:", pkgs, value = TRUE)
  pkgs <- gsub("^package:", "", pkgs)
  pkgs
}

## Adopted R.utils 2.1.0 (2015-06-15)
#' @importFrom utils capture.output
capture_output <- function(expr, envir = parent.frame(), ...) {
  res <- eval({
    file <- rawConnection(raw(0L), open = "w")
    on.exit(close(file))
    capture.output(expr, file = file)
    rawToChar(rawConnectionValue(file))
  }, envir = envir, enclos = baseenv())
  unlist(strsplit(res, split = "\n", fixed = TRUE), use.names = FALSE)
}

printf <- function(...) cat(sprintf(...))


## Tests if the current OS is of a certain type
is_os <- function(name) {
  if (name == "windows") {
    return(.Platform$OS.type == "windows")
  } else {
    grepl(paste0("^", name), R.version$os)
  }
}


## From R.utils 2.5.0
tempvar <- function(prefix = "var", value = NA, envir = parent.frame()) {
  max_tries <- 1e6
  max_int <- .Machine$integer.max

  ii <- 0L
  while (ii < max_tries) {
    # Generate random variable name
    idx <- sample.int(max_int, size = 1L)
    name <- sprintf("%s%d", prefix, idx)

    # Available?
    if (!exists(name, envir = envir, inherits = FALSE)) {
      assign(name, value, envir = envir, inherits = FALSE)
      return(name)
    }

    ii <- ii + 1L
  }

  # Failed to find a unique temporary variable name
  stopf("Failed to generate a unique non-existing temporary variable with prefix '%s'", prefix) #nolint
}


result_has_errors <- function(result) {
  stop_if_not(inherits(result, "FutureResult"))

  for (c in result$conditions) {
    if (inherits(c$condition, "error")) return(TRUE)
  }
  
  FALSE
}


#' @importFrom utils file_test
file_info <- function(file) {
  if (is.null(file) || is.na(file)) return("<NA>")
  if (file_test("-f", file)) {
    info <- sprintf("%d bytes", file.info(file)$size)
    n <- length(readLines(file, warn = FALSE))
    info <- sprintf("%s; %d lines", info, n)
  } else {
    info <- "<non-existing>"
  }
  sprintf("%s (%s)", sQuote(file), info)
}


#' @importFrom utils file_test
dir_info <- function(dir) {
  if (is.null(dir) || is.na(dir)) return("<NA>")
  if (file_test("-d", dir)) {
    files <- dir(path = dir, full.names = TRUE)
    info <- file.info(files)
    info <- info[info[["isdir"]], "mtime"]
    if (length(info) == 0) {
      info <- "0 folders"
    } else {
      period <- range(info)
      period <- format(period, "%FT%T")
      info <- sprintf("%d folders; %s/%s", length(info), period[1], period[2])
    }
  } else {
    info <- "<non-existing>"
  }
  sprintf("%s (%s)", sQuote(dir), info)
}

