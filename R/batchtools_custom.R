#' Batchtools futures for custom batchtools configuration
#'
#' @inheritParams BatchtoolsFutureBackend
#'
#' @param \ldots Additional arguments passed to [BatchtoolsFutureBackend()].
#'
#' @return An object of class `BatchtoolsFuture`.
#'
#' @examplesIf interactive()
#' library(future)
#'
#' ## Create custom cluster functions (here same as "local")
#' cf <- batchtools::makeClusterFunctionsInteractive(external = TRUE)
#' print(cf)
#' str(cf)
#'
#' # Use custom batchtools backend
#' plan(future.batchtools::batchtools_custom, cluster.functions = cf)
#' print(plan())
#'
#' message("Main process ID: ", Sys.getpid())
#'
#' f <- future(Sys.getpid())
#' pid <- value(f)
#' message("Worker process ID: ", pid)
#'
#' @rdname BatchtoolsFutureBackend
#' @keywords internal
#'
#' @export
#' @importFrom batchtools findConfFile
BatchtoolsCustomFutureBackend <- function(...) {
  assert_no_positional_args_but_first()

  core <- BatchtoolsMultiprocessFutureBackend(
    ...
  )
  
  core[["futureClasses"]] <- c("BatchtoolsCustomFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsCustomFutureBackend", class(core)))
  core
}

#' @inheritParams BatchtoolsCustomFutureBackend
#'
#' @export
batchtools_custom <- function(...) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_custom() must never be called directly")
}
class(batchtools_custom) <- c(
  "batchtools_custom", "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_custom, "init") <- TRUE
attr(batchtools_custom, "tweakable") <- c("cluster.functions", "conf.file", "registry", "resources", "finalize")
attr(batchtools_custom, "factory") <- BatchtoolsCustomFutureBackend

