#' batchtools multicore futures
#'
#' A batchtools multicore future is an asynchronous multiprocess
#' future that will be evaluated in a background R session.\cr
#' \cr
#' _We highly recommend using [future::multisession]
#' (sic!) futures of the \pkg{future} package instead of
#' multicore batchtools futures._
#'
#' @inheritParams BatchtoolsFutureBackend
#'
#' @param workers The number of multicore processes to be
#' available for concurrent batchtools multicore futures.
#'
#' @param \ldots Additional arguments passed
#' to [BatchtoolsFutureBackend()].
#'
#' @return An object of class `BatchtoolsMulticoreFuture`.
#'
#' @details
#' batchtools multicore futures rely on the batchtools backend set
#' up by [batchtools::makeClusterFunctionsMulticore()].
#' The batchtools multicore backend only works on operating systems
#' supporting the `ps` command-line tool, e.g. Linux and macOS.
#'
#' @importFrom batchtools makeClusterFunctionsMulticore
#' @importFrom parallelly availableCores supportsMulticore
#' @keywords internal
#' @export
BatchtoolsMulticoreFutureBackend <- function(workers = availableCores(constraints = "multicore"), ...) {
  assert_no_positional_args_but_first()

  if (is.function(workers)) workers <- workers()
  if (is.null(workers)) {
    workers <- getOption("future.batchtools.workers", default = 100L)
  }
  stop_if_not(
    is.numeric(workers),
    length(workers) == 1,
    !is.na(workers), workers >= 1
  )

  ## Fall back to batchtools_local if multicore processing is not supported
  if ((workers == 1L && !inherits(workers, "AsIs")) || !supportsMulticore(warn = TRUE)) {
    return(BatchtoolsLocalFutureBackend(...))
  }

  oopts <- options(mc.cores = workers)
  on.exit(options(oopts))

  cluster.functions <- makeClusterFunctionsMulticore(ncpus = workers)

  core <- BatchtoolsMultiprocessFutureBackend(
    workers = workers,
    cluster.functions = cluster.functions,
    ...
  )
  
  core[["futureClasses"]] <- c("BatchtoolsMulticoreFuture", "BatchtoolsMultiprocessFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsMulticoreFutureBackend", class(core)))
  core
}


#' @rdname BatchtoolsMulticoreFutureBackend
#' @export
batchtools_multicore <- function(..., workers = availableCores(constraints = "multicore"), envir = parent.frame()) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_multicore() must never be called directly")
}
class(batchtools_multicore) <- c(
  "batchtools_multicore", "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_multicore, "tweakable") <- c("workers", "finalize")
attr(batchtools_multicore, "init") <- TRUE
attr(batchtools_multicore, "factory") <- BatchtoolsMulticoreFutureBackend
