#' batchtools local and interactive futures
#'
#' A batchtools local future is an synchronous uniprocess future that
#' will be evaluated in a background R session.
#' A batchtools interactive future is an synchronous uniprocess future
#' that will be evaluated in the current R session (and variables will
#' be assigned to the calling environment rather than to a local one).
#' Both types of futures will block until the futures are resolved.
#'
#' @inheritParams BatchtoolsFutureBackend
#' @inheritParams batchtools::makeClusterFunctions
#' 
#' @param \ldots Additional arguments passed to [BatchtoolsFutureBackend()].
#'
#' @return An object of class `BatchtoolsUniprocessFuture`.
#'
#' @details
#' batchtools local futures rely on the batchtools backend set up by
#' \code{\link[batchtools:makeClusterFunctionsInteractive]{batchtools::makeClusterFunctionsInteractive(external = TRUE)}}
#' and batchtools interactive futures on the one sQet up by
#' [batchtools::makeClusterFunctionsInteractive()].
#' These are supported by all operating systems.
#'
#' An alternative to batchtools local futures is to use
#' [cluster][future::cluster] futures of the \pkg{future}
#' package with a single local background session, i.e.
#' `plan(cluster, workers = "localhost")`.
#'
#' An alternative to batchtools interactive futures is to use
#' `plan(sequential, split = TRUE)` futures of the \pkg{future} package.
#'
#' @rdname BatchtoolsFutureBackend
#' @keywords internal
#'
#' @importFrom batchtools makeClusterFunctionsInteractive
#' @aliases BatchtoolsLocalFutureBackend BatchtoolsBashFutureBackend
#' @export
BatchtoolsLocalFutureBackend <- function(fs.latency = 0.0, ...) {
  assert_no_positional_args_but_first()

  core <- BatchtoolsUniprocessFutureBackend(
    cluster.functions = makeClusterFunctionsInteractive(fs.latency = fs.latency, external = TRUE),
    ...
  )

  core[["futureClasses"]] <- c("BatchtoolsLocalFuture", "BatchtoolsUniprocessFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsLocalFutureBackend", class(core)))
  core
}


#' A batchtools backend that resolves futures sequentially in transient background R sessions
#'
#' The batchtools local backend is useful for verifying parts of your
#' \pkg{batchtools} setup locally, before using a more advanced backend such
#' as the job-scheduler backends.
#'
#' @inheritParams BatchtoolsFutureBackend
#' @inheritParams BatchtoolsLocalFutureBackend
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools local futures use \pkg{batchtools} cluster functions
#' created by [batchtools::makeClusterFunctionsInteractive()] with
#' `external = TRUE`.
#'
#' An alternative to the batchtools interactive backend is to use
#' `plan(future::cluster, workers = I(1))`.
#'
#' @examples
#' plan(future.batchtools::batchtools_local)
#'
#' message("Main process ID: ", Sys.getpid())
#'
#' f <- future(Sys.getpid())
#' pid <- value(f)
#' message("Worker process ID: ", pid)
#' 
#' @export
batchtools_local <- function(..., fs.latency = 0.0) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_local() must never be called directly")
}
class(batchtools_local) <- c(
  "batchtools_local", "batchtools_uniprocess", "batchtools",
  "uniprocess", "future", "function"
)
attr(batchtools_local, "tweakable") <- c("finalize")
attr(batchtools_local, "untweakable") <- c("workers")
attr(batchtools_local, "init") <- TRUE
attr(batchtools_local, "factory") <- BatchtoolsLocalFutureBackend
