#' @inheritParams BatchtoolsFuture
#'
#' @importFrom batchtools makeClusterFunctionsInteractive
#' @export
BatchtoolsInteractiveFutureBackend <- function(...) {
  assert_no_positional_args_but_first()

  core <- BatchtoolsUniprocessFutureBackend(
    cluster.functions = makeClusterFunctionsInteractive(external = FALSE),
    ...
  )

  core[["futureClasses"]] <- c("BatchtoolsInteractiveFuture", "BatchtoolsUniprocessFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsInteractiveFutureBackend", class(core)))
  core
}

#' @export
batchtools_interactive <- function(..., envir = parent.frame()) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_interactive() must never be called directly")
}
class(batchtools_interactive) <- c(
  "batchtools_interactive", "batchtools_uniprocess", "batchtools",
  "uniprocess", "future", "function"
)
attr(batchtools_interactive, "tweakable") <- c("finalize")
attr(batchtools_interactive, "untweakable") <- c("workers")
attr(batchtools_interactive, "init") <- TRUE
attr(batchtools_interactive, "factory") <- BatchtoolsInteractiveFutureBackend


