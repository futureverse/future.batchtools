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
#'
#' @param workers The number of SSH processes to be
#' available for concurrent batchtools SSH futures.
#' @param \ldots Additional arguments passed
#' to [BatchtoolsFutureBackend()].
#'
#' @return An object of class `BatchtoolsMulticoreFuture`.
#'
#' @details
#' batchtools SSH futures rely on the batchtools backend set
#' up by [batchtools::makeClusterFunctionsSSH()].
#' The batchtools SSH backend only works on operating systems
#' supporting the `ssh` and `ps` command-line tool, e.g. Linux and macOS.
#'
#' @importFrom parallelly availableWorkers
#'
#' @keywords internal
#' @importFrom batchtools makeClusterFunctionsSSH
#' @importFrom parallelly availableCores
#' @export
BatchtoolsSSHFutureBackend <- function(workers = availableWorkers(), ...) {
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
  args <- c(list(workers = ssh_worker), dotdotdot[keep])
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


#' @export
batchtools_ssh <- function(..., workers = availableWorkers(), envir = parent.frame()) {
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
