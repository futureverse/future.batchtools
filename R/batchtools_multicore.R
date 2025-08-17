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
#' @inheritParams batchtools::makeClusterFunctions
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
#' @keywords internal
#'
#' @aliases batchtools_custom
#' @importFrom batchtools makeClusterFunctionsMulticore
#' @importFrom parallelly availableCores supportsMulticore
#' @importFrom tools pskill
#' @export
BatchtoolsMulticoreFutureBackend <- function(workers = availableCores(constraints = "multicore"), fs.latency = 0.0, ...) {
  assert_no_positional_args_but_first()

  if (is.function(workers)) workers <- workers()
  stop_if_not(
    is.numeric(workers),
    length(workers) == 1,
    !is.na(workers),
    is.finite(workers),
    workers >= 1
  )

  ## Fall back to batchtools_local if multicore processing is not supported
  if ((workers == 1L && !inherits(workers, "AsIs")) || !supportsMulticore(warn = TRUE)) {
    return(BatchtoolsLocalFutureBackend(..., fs.latency = fs.latency))
  }

  oopts <- options(mc.cores = workers)
  on.exit(options(oopts))

  cluster.functions <- makeClusterFunctionsMulticore(ncpus = workers, fs.latency = fs.latency)
  cluster.functions$killJob <- function(reg, batch.id) {
    pid <- as.integer(batch.id)
    if (is.na(pid) || pid <= 0) return(FALSE)
    pskill(pid)
  }

  core <- BatchtoolsMultiprocessFutureBackend(
    workers = workers,
    cluster.functions = cluster.functions,
    ...
  )
  
  core[["futureClasses"]] <- c("BatchtoolsMulticoreFuture", "BatchtoolsMultiprocessFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsMulticoreFutureBackend", class(core)))
  core
}


#' A batchtools backend that resolves futures in parallel via forked background R processes
#'
#' @inheritParams BatchtoolsMulticoreFutureBackend
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools multicore futures use \pkg{batchtools} cluster functions
#' created by [batchtools::makeClusterFunctionsMulticore()] with
#' `ncpus = workers`.
#'
#' An alternative to the batchtools multicore backend is to use
#' `plan(future::multicore)`.
#'
#' @examplesIf interactive()
#' library(future)
#' plan(future.batchtools::batchtools_multicore, workers = 2)
#'
#' message("Main process ID: ", Sys.getpid())
#'
#' f <- future({
#'   data.frame(
#'     hostname = Sys.info()[["nodename"]],
#'           os = Sys.info()[["sysname"]],
#'        cores = unname(parallelly::availableCores()),
#'          pid = Sys.getpid(),
#'      modules = Sys.getenv("LOADEDMODULES")
#'   )
#' })
#' info <- value(f)
#' print(info)
#' 
#' @export
batchtools_multicore <- function(..., workers = availableCores(constraints = "multicore"), fs.latency = 0.0) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_multicore() must never be called directly")
}
class(batchtools_multicore) <- c(
  "batchtools_multicore", "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_multicore, "tweakable") <- c("workers", "finalize")
attr(batchtools_multicore, "init") <- TRUE
attr(batchtools_multicore, "factory") <- BatchtoolsMulticoreFutureBackend
