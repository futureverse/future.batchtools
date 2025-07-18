#' @export
BatchtoolsOpenLavaFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "openlava")
  core[["futureClasses"]] <- c("BatchtoolsOpenLavaFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsOpenLavaFutureBackend", class(core)))
  core
}


#' A batchtools openlava backend resolves futures in parallel via a OpenLava job scheduler
#'
#' @inheritParams BatchtoolsFutureBackend
#' @inheritParams BatchtoolsTemplateFutureBackend
#'
#' @param template (optional) Name of job-script template to be searched
#' for by [batchtools::findTemplateFile()]. If not found, it defaults to
#' the `templates/openlava.tmpl` part of this package (see below).
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools openlava futures use \pkg{batchtools} cluster functions
#' created by [batchtools::makeClusterFunctionsOpenLava()], which requires
#' that OpenLava commands `sbatch`, `squeue`, and `scancel` are installed on
#' the current machine.
#'
#' The default template script `templates/openlava.tmpl` can be found in:
#'
#' ```r
#' system.file("templates", "openlava.tmpl", package = "future.batchtools")
#' ```
#'
#' and comprise:
#'
#' `r paste(c("\x60\x60\x60bash", readLines("inst/templates/openlava.tmpl"), "\x60\x60\x60"), collapse = "\n")`
#'
#' @examplesIf interactive()
#' # Limit runtime to 3 minutes and memory to 200 MiB per future
#' plan(batchtools_lsf, resources = list(W = "00:03:00", M = "200"))
#'
#' message("Main process ID: ", Sys.getpid())
#'
#' f <- future(Sys.getpid())
#' pid <- value(f)
#' message("Worker process ID: ", pid)
#'
#' @references
#' * <https://en.wikipedia.org/wiki/OpenLava>
#'
#' @export
batchtools_openlava <- function(..., template = "openlava", scheduler.latency = 1.0, fs.latency = 65.0) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_openlava() must never be called directly")
}
class(batchtools_openlava) <- c(
  "batchtools_openlava", "batchtools_template",
  "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_openlava, "tweakable") <- c(
  "workers",
  "finalize",
  ## Arguments to batchtools::makeClusterFunctionsOpenLava()
  "scheduler.latency", "fs.latency"
)
attr(batchtools_openlava, "init") <- TRUE
attr(batchtools_openlava, "factory") <- BatchtoolsOpenLavaFutureBackend
