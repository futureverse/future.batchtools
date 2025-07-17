#' @export
BatchtoolsLsfFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "lsf")
  core[["futureClasses"]] <- c("BatchtoolsLsfFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsLsfFutureBackend", class(core)))
  core
}


#' A batchtools LSF backend resolves futures in parallel via a Load Sharing Facility (LSF) job scheduler
#'
#' @inheritParams BatchtoolsFutureBackend
#' @inheritParams BatchtoolsTemplateFutureBackend
#'
#' @param template (optional) Name of job-script template to be searched
#' for by [batchtools::findTemplateFile()]. If not found, it defaults to
#' the `templates/lsf.tmpl` part of this package (see below).
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools lsf futures use \pkg{batchtools} cluster functions
#' created by [batchtools::makeClusterFunctionsLSF()], which requires
#' that LSF commands `bsub`, `bjobs`, and `bkill` are installed on
#' the current machine.
#'
#' The default template script `templates/lsf.tmpl` can be found in:
#'
#' ```r
#' system.file("templates", "lsf.tmpl", package = "future.batchtools")
#' ```
#'
#' and comprise:
#'
#' `r paste(c("\x60\x60\x60bash", readLines("inst/templates/lsf.tmpl"), "\x60\x60\x60"), collapse = "\n")`
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
#' * <https://en.wikipedia.org/wiki/IBM_Spectrum_LSF>
#'
#' @export
batchtools_lsf <- function(..., template = "lsf") {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_lsf() must never be called directly")
}
class(batchtools_lsf) <- c(
  "batchtools_lsf", "batchtools_template",
  "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_lsf, "tweakable") <- c(
  "workers",
  "finalize",
  ## Arguments to batchtools::makeClusterFunctionsLSF()
  "scheduler.latency", "fs.latency"
)
attr(batchtools_lsf, "init") <- TRUE
attr(batchtools_lsf, "factory") <- BatchtoolsLsfFutureBackend
