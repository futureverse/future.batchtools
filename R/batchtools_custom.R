#' Batchtools futures for custom batchtools configuration
#'
#' @inheritParams BatchtoolsFutureBackend
#'
#' @param \ldots Additional arguments passed to [BatchtoolsFutureBackend()].
#'
#' @return An object of class `BatchtoolsFuture`.
#'
#' @example incl/batchtools_custom.R
#'
#' @aliases batchtools_custom
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

#' @export
batchtools_custom <- function(..., envir = parent.frame()) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_custom() must never be called directly")
}
class(batchtools_custom) <- c(
  "batchtools_custom", "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_custom, "init") <- TRUE
attr(batchtools_custom, "tweakable") <- c("cluster.functions", "conf.file", "registry", "resources", "finalize")
attr(batchtools_custom, "factory") <- BatchtoolsCustomFutureBackend

