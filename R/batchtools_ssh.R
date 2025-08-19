#' batchtools SSH futures
#'
#' A batchtools SSH future is an asynchronous multiprocess
#' future that will be evaluated in a background R session.\cr
#' \cr
#' _We highly recommend using [future::multisession]
#' (sic!) futures of the \pkg{future} package instead of
#' SSH batchtools futures._
#'
#' @inheritParams BatchtoolsFutureBackend
#' @inheritParams batchtools::makeClusterFunctions
#'
#' @return An object of class `BatchtoolsMulticoreFuture`.
#'
#' @details
#' batchtools SSH futures rely on the batchtools backend set
#' up by [batchtools::makeClusterFunctionsSSH()].
#' The batchtools SSH backend only works on operating systems
#' supporting the `ssh` and `ps` command-line tool, e.g. Linux and macOS.
#'
#' @rdname BatchtoolsFutureBackend
#' @keywords internal
#'
#' @importFrom parallelly availableWorkers
#' @importFrom batchtools makeClusterFunctionsSSH
#' @importFrom parallelly availableCores
#' @export
BatchtoolsSSHFutureBackend <- function(workers = availableWorkers(), fs.latency = 65.0, ...) {
  assert_no_positional_args_but_first()

  if (is.function(workers)) workers <- workers()
  stop_if_not(
    is.character(workers),
    length(workers) > 0,
    !anyNA(workers),
    all(nzchar(workers))
  )

  dotdotdot <- list(...)

  ## WORKAROUND: 'max.load' cannot be +Inf, because that'll lead to:
  ##
  ## Error in sample.int(x, size, replace, prob) : 
  ##   too few positive probabilities
  ##
  ## in the submitJob() function created by makeClusterFunctionsSSH().
  ## /HB 2022-12-12
  ssh_worker <- list(Worker$new(
    "localhost",
    ncpus = 1L,
    max.load = .Machine$double.xmax  ## +Inf fails
  ))

  keep <- which(names(dotdotdot) %in% names(formals(makeClusterFunctionsSSH)))
  args <- c(list(workers = ssh_worker), dotdotdot[keep], fs.latency = fs.latency)
  cluster.functions <- do.call(makeClusterFunctionsSSH, args = args)

  ## Drop used '...' arguments
  if (length(keep) > 0) dotdotdot <- dotdotdot[-keep]

  args <- list(
    workers = workers,
    cluster.functions = cluster.functions
  )
  args <- c(args, dotdotdot)

  core <- do.call(BatchtoolsMultiprocessFutureBackend, args = args)
  core[["futureClasses"]] <- c("BatchtoolsSSHFuture", "BatchtoolsMultiprocessFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsSSHFutureBackend", class(core)))
  core
}


#' A batchtools backend that resolves futures in parallel via background R sessions over SSH
#'
#' @inheritParams BatchtoolsSSHFutureBackend
#'
#' @details
#' The `batchtools_ssh` backend uses the batchtools backend set
#' up by [batchtools::makeClusterFunctionsSSH()], which requires
#' system commands `ssh` and `ps` as available on Linux and macOS.
#'
#' An alternative to `batchtools_ssh` is to use
#' [cluster][future::cluster] futures of the \pkg{future}
#' package with a single local background session, i.e.
#' `plan(cluster, workers = c("localhost"))`.
#'
#' @inheritParams BatchtoolsSSHFutureBackend
#'
#' @keywords internal
#' @export
batchtools_ssh <- function(..., workers = availableWorkers(), fs.latency = 65.0, delete = "on-success") {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_ssh() must never be called directly")
}
class(batchtools_ssh) <- c(
  "batchtools_ssh", "batchtools_custom",
  "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_ssh, "tweakable") <- c("finalize")
attr(batchtools_ssh, "init") <- TRUE
attr(batchtools_ssh, "factory") <- BatchtoolsSSHFutureBackend
