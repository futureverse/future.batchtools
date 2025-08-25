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
nbrOfWorkers.BatchtoolsFutureBackend <- function(evaluator) {
  backend <- evaluator

  ## 1. Infer from 'workers' argument
  workers <- backend[["workers"]]
  if (!is.null(workers)) {
    if (is.function(workers)) workers <- workers()
    stop_if_not(length(workers) >= 1)
    if (is.numeric(workers)) return(prod(workers))
    if (is.character(workers)) return(length(workers))
    stop("Invalid data type of 'workers': ", mode(workers))
  }

  ## 2. Infer from 'cluster.functions' argument
  cluster.functions <- backend[["cluster.functions"]]
  if (!is.null(cluster.functions)) {
    stop_if_not(inherits(cluster.functions, "ClusterFunctions"))

    name <- cluster.functions$name
    if (is.null(name)) name <- cluster.functions$Name

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
nbrOfWorkers.BatchtoolsUniprocessFutureBackend <- function(evaluator) {
  assert_no_positional_args_but_first()
  1L
}

#' @export
nbrOfWorkers.BatchtoolsMulticoreFutureBackend <- function(evaluator) {
  assert_no_positional_args_but_first()

  backend <- evaluator

  ## 1. Infer from 'workers' argument
  workers <- backend[["workers"]]
  if (is.function(workers)) workers <- workers()
  stop_if_not(length(workers) == 1L, is.numeric(workers), !is.na(workers), is.finite(workers), workers >= 1)
  workers
}


#' @importFrom future nbrOfWorkers nbrOfFreeWorkers
#' @export
nbrOfFreeWorkers.BatchtoolsFutureBackend <- function(evaluator, background = FALSE, ...) {
  backend <- evaluator

  ## Special case #1: Fall back to uniprocess processing
  if (inherits(backend, "BatchtoolsUniprocessFutureBackend")) {
    return(NextMethod())
  }
  
  ## Special case #2: Infinite number of workers
  workers <- nbrOfWorkers(backend)
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
nbrOfFreeWorkers.BatchtoolsUniprocessFutureBackend <- function(evaluator, background = FALSE, ...) {
  assert_no_positional_args_but_first()
  if (isTRUE(background)) 0L else 1L
}



#' @export
nbrOfFreeWorkers.BatchtoolsMultiprocessFutureBackend <- function(evaluator, background = FALSE, ...) {
  assert_no_positional_args_but_first()

  backend <- evaluator

  workers <- nbrOfWorkers(backend)
  
  ## Special case: Infinite number of workers
  if (is.infinite(workers)) return(workers)

  usedWorkers <- length(FutureRegistry(backend[["reg"]], action = "list"))
  
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
