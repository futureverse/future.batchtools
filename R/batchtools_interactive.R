#' @rdname BatchtoolsFutureBackend
#' @keywords internal
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

#' A batchtools backend that resolves futures sequentially in the current R session
#'
#' The batchtools interactive backend is useful for verifying parts of your
#' \pkg{batchtools} setup locally, while still being able to do interactive
#' debugging.
#'
#' @inheritParams BatchtoolsFutureBackend
#' @inheritParams BatchtoolsInteractiveFutureBackend
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools interactive futures use \pkg{batchtools} cluster functions
#' created by [batchtools::makeClusterFunctionsInteractive()] with
#' `external = TRUE`.
#'
#' An alternative to the batchtools interactive backend is to use
#' `plan(future::sequential)`, which is a faster way process futures
#' sequentially and that also can be debugged interactively.
#'
#' @examples
#' plan(batchtools_interactive)
#'
#' message("Main process ID: ", Sys.getpid())
#'
#' f <- future(Sys.getpid())
#' pid <- value(f)
#' message("Worker process ID: ", pid)
#' 
#' @inheritParams BatchtoolsInteractiveFutureBackend
#'
#' @export
batchtools_interactive <- function(...) {
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
