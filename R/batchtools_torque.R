#' @export
BatchtoolsTorqueFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "torque")
  core[["futureClasses"]] <- c("BatchtoolsTorqueFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsTorqueFutureBackend", class(core)))
  core
}

#' A batchtools TORQUE backend resolves futures in parallel via a TORQUE/PBS job scheduler
#'
#' @inheritParams BatchtoolsTemplateFutureBackend
#'
#' @param template (optional) Name of job-script template to be searched
#' for by [batchtools::findTemplateFile()]. If not found, it defaults to
#' the `templates/torque.tmpl` part of this package (see below).
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools torque futures use \pkg{batchtools} cluster functions
#' created by [batchtools::makeClusterFunctionsTORQUE()], which requires
#' that TORQUE commands `qsub`, `qselect`, and `qdel` are installed on
#' the current machine.
#'
#' The default template script `templates/torque.tmpl` can be found in:
#'
#' ```r
#' system.file("templates", "torque.tmpl", package = "future.batchtools")
#' ```
#'
#' and comprise:
#'
#' `r paste(c("\x60\x60\x60bash", readLines("inst/templates/torque.tmpl"), "\x60\x60\x60"), collapse = "\n")`
#'
#' @examplesIf interactive()
#' library(future)
#'
#' # Limit runtime to 3 minutes and memory to 200 MiB per future
#' plan(future.batchtools::batchtools_torque, resources = list(walltime = "00:03:00", mem = "200mb"))
#'
#' f <- future(Sys.info())
#' info <- value(f)
#' print(info)
#' 
#' @references
#' * <https://en.wikipedia.org/wiki/TORQUE>
#'
#' @export
batchtools_torque <- function(..., template = "torque", scheduler.latency = 1.0, fs.latency = 65.0, resources = list(), workers = getOption("future.batchtools.workers", default = 100L)) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_torque() must never be called directly")
}
class(batchtools_torque) <- c(
  "batchtools_torque", "batchtools_template",
  "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_torque, "tweakable") <- c(
  "workers",
  "finalize",
  ## Arguments to batchtools::makeClusterFunctionsTORQUE()
  "scheduler.latency", "fs.latency"
)
attr(batchtools_torque, "init") <- TRUE
attr(batchtools_torque, "factory") <- BatchtoolsTorqueFutureBackend
