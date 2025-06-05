is_na <- function(x) {
  if (length(x) != 1L) return(FALSE)
  is.na(x)
}

is_false <- function(x) {
  if (length(x) != 1L) return(FALSE)
  x <- as.logical(x)
  x <- unclass(x)
  identical(FALSE, x)
}

stop_if_not <- function(...) {
  res <- list(...)
  for (ii in 1L:length(res)) {
    res_ii <- .subset2(res, ii)
    if (length(res_ii) != 1L || is.na(res_ii) || !res_ii) {
        mc <- match.call()
        call <- deparse(mc[[ii + 1]], width.cutoff = 60L)
        if (length(call) > 1L) call <- paste(call[1L], "....")
        stopf("%s is not TRUE", sQuote(call), call. = FALSE, domain = NA)
    }
  }
  
  NULL
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

## From R.utils 2.0.2 (2015-05-23)
hpaste <- function(..., sep="", collapse=", ", last_collapse=NULL,
                   max_head=if (missing(last_collapse)) 3 else Inf,
                   max_tail=if (is.finite(max_head)) 1 else Inf,
                   abbreviate="...") {
  max_head <- as.double(max_head)
  max_tail <- as.double(max_tail)
  if (is.null(last_collapse)) last_collapse <- collapse

  # Build vector 'x'
  x <- paste(..., sep = sep)
  n <- length(x)

  # Nothing todo?
  if (n == 0) return(x)
  if (is.null(collapse)) return(x)

  # Abbreviate?
  if (n > max_head + max_tail + 1) {
    head <- x[seq_len(max_head)]
    tail <- rev(rev(x)[seq_len(max_tail)])
    x <- c(head, abbreviate, tail)
    n <- length(x)
  }

  if (!is.null(collapse) && n > 1) {
    if (last_collapse == collapse) {
      x <- paste(x, collapse = collapse)
    } else {
      x_head <- paste(x[1:(n - 1)], collapse = collapse)
      x <- paste(x_head, x[n], sep = last_collapse)
    }
  }

  x
}

## Adopted from R.oo 1.19.0 (2015-06-15)
trim <- function(x, ...) {
  sub("[\t\n\f\r ]*$", "", sub("^[\t\n\f\r ]*", "", x))
}

comma <- function(x, sep = ", ") paste(x, collapse = sep)

commaq <- function(x, sep = ", ") paste(sQuote(x), collapse = sep)

## Evaluates an expression in global environment.
## Because geval() is exported, we want to keep its environment()
## as small as possible, which is why we use local().  Without,
## the environment would be that of the package itself and all of
## the package would be exported.
geval <- local(function(expr, substitute = FALSE, envir = .GlobalEnv, enclos = baseenv(), ...) {
  if (substitute) expr <- substitute(expr)
  eval(expr, envir = envir, enclos = enclos)
})


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


assert_no_positional_args_but_first <- function(call = sys.call(sys.parent())) {
  ast <- as.list(call)
  if (length(ast) <= 2L) return()
  ast <- ast[-(1:2)]
  dots <- vapply(ast, FUN = identical, as.symbol("..."), FUN.VALUE = FALSE)
  ast <- ast[!dots]
  if (length(ast) == 0L) return()
  names <- names(ast)
  if (is.null(names) || any(names == "")) {    
    stopf("Function %s() requires that all arguments beyond the first one are passed by name and not by position: %s", as.character(call[[1L]]), deparse(call, width.cutoff = 100L))
  }
}
