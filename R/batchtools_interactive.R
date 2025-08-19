#' @rdname BatchtoolsFutureBackend
#' @keywords internal
#'
#' @importFrom batchtools makeClusterFunctionsInteractive
#' @export
BatchtoolsInteractiveFutureBackend <- function(fs.latency = 0.0, ...) {
  assert_no_positional_args_but_first()

  args <- list(...)
  if ("workers" %in% names(args)) {
    stop("Unknown argument 'workers'")
  }

  core <- BatchtoolsUniprocessFutureBackend(
    cluster.functions = makeClusterFunctionsInteractive(fs.latency = fs.latency, external = FALSE),
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
#' library(future)
#' plan(future.batchtools::batchtools_interactive)
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
#' @inheritParams BatchtoolsInteractiveFutureBackend
#'
#' @export
batchtools_interactive <- function(..., fs.latency = 0.0, delete = "on-success") {
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
