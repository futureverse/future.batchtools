#' Gets the number of batchtools workers
#'
#' Tries to infer the total number of batchtools workers.  This is
#' done using various ad-hoc procedures based on code inspection
#' of batchtools itself.
#'
#' @param evaluator A future evaluator function.
#' If NULL (default), the current evaluator as returned
#' by [future::plan()] is used.
#'
#' @return A number in \eqn{[1, Inf]}.
#'
#' @importFrom future nbrOfWorkers
#' @export
#' @keywords internal
nbrOfWorkers.batchtools <- function(evaluator) {
  ## 1. Infer from 'workers' argument
  expr <- formals(evaluator)$workers
  workers <- eval(expr, enclos = baseenv())
  if (!is.null(workers)) {
    if (is.function(workers)) workers <- workers()
    stop_if_not(length(workers) >= 1)
    if (is.numeric(workers)) return(prod(workers))
    if (is.character(workers)) return(length(workers))
    stop("Invalid data type of 'workers': ", mode(workers))
  }

  ## 2. Infer from 'cluster.functions' argument
  expr <- formals(evaluator)$cluster.functions
  cf <- eval(expr, enclos = baseenv())
  if (!is.null(cf)) {
    stop_if_not(inherits(cf, "ClusterFunctions"))

    name <- cf$name
    if (is.null(name)) name <- cf$Name

    ## Uni-process backends
    if (name %in% c("Local", "Interactive")) return(1L)

    ## Cluster backends (with a scheduler queue)
    if (name %in% c("TORQUE", "Slurm", "SGE", "OpenLava", "LSF")) {
      return(availableHpcWorkers())
    }
  }

  ## If still not known, assume a generic HPC scheduler
  availableHpcWorkers()
}

#' @export
nbrOfWorkers.batchtools_uniprocess <- function(evaluator) {
  assert_no_positional_args_but_first()
  1L
}

#' @export
nbrOfWorkers.batchtools_multicore <- function(evaluator) {
  ## 1. Infer from 'workers' argument
  expr <- formals(evaluator)$workers
  workers <- eval(expr, enclos = baseenv())
  if (is.function(workers)) workers <- workers()
  stop_if_not(length(workers) == 1L, is.numeric(workers), !is.na(workers), is.finite(workers), workers >= 1)
  workers
}

#' @importFrom future nbrOfWorkers nbrOfFreeWorkers
#' @export
nbrOfFreeWorkers.batchtools <- function(evaluator, background = FALSE, ...) {
  ## Special case #1: Fall back to uniprocess processing
  if (inherits(evaluator, "uniprocess")) {
    return(NextMethod())
  }
  
  ## Special case #2: Infinite number of workers
  workers <- nbrOfWorkers(evaluator)
  if (is.infinite(workers)) return(workers)

  ## In all other cases, we need to figure out how many workers
  ## are running at the moment
  
  warnf("nbrOfFreeWorkers() for %s is not fully implemented. For now, it'll assume that none of the workers are occupied", setdiff(class(evaluator), c("FutureStrategy", "tweaked"))[1])
  usedWorkers <- 0L  ## Mockup for now
  
  workers <- workers - usedWorkers
  stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 0L)
  workers
}


#' @export
nbrOfFreeWorkers.batchtools_uniprocess <- function(evaluator, background = FALSE, ...) {
  assert_no_positional_args_but_first()
  if (isTRUE(background)) 0L else 1L
}



#' @export
nbrOfFreeWorkers.batchtools_multiprocess <- function(evaluator, background = FALSE, ...) {
  assert_no_positional_args_but_first()

  workers <- nbrOfWorkers(evaluator)

  ## Create a dummy future
  future <- evaluator(NULL, globals = FALSE, lazy = TRUE)
  freg <- sprintf("workers-%s", class(future)[1])
  usedWorkers <- length(FutureRegistry(freg, action = "list"))
  
  workers <- workers - usedWorkers
  stop_if_not(length(workers) == 1L, !is.na(workers), workers >= 0L)
  workers
}



## Number of available workers in an HPC environment
##
## @return (numeric) A positive integer or `+Inf`.
availableHpcWorkers <- function() {
  name <- "future.batchtools.workers"
  value <- getOption(name, default = 100)
  if (!is.numeric(value) || length(value) != 1L ||
      is.na(value) || value < 1.0) {
    stopf("Option %s does not specify a value >= 1: %s",
          sQuote(name), paste(sQuote(value), collapse = ", "))
  }
  value <- floor(value)
  value
}
