#' @export
BatchtoolsLsfFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "lsf")
  core[["futureClasses"]] <- c("BatchtoolsLsfFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsLsfFutureBackend", class(core)))
  core
}


#' A batchtools LSF backend resolves futures in parallel via a Load Sharing Facility (LSF) job scheduler
#'
#' @inheritParams BatchtoolsTemplateFutureBackend
#'
#' @param template (optional) Name of job-script template to be searched
#' for by [batchtools::findTemplateFile()]. If not found, it defaults to
#' the `templates/lsf.tmpl` part of this package (see below).
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools Load Sharing Facility (LSF) futures use \pkg{batchtools}
#' cluster functions created by [batchtools::makeClusterFunctionsLSF()],
#' which requires that LSF commands `bsub`, `bjobs`, and `bkill` are
#' installed on the current machine.
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
#' library(future)
#'
#' # Limit runtime to 10 minutes and total memory to 400 MiB per future,
#' # request a parallel environment with four slots on a single host.
#' # Submit to the 'freecycle' queue. Load environment modules 'r' and
#' # 'jags'. Report on job details at startup and at the end of the job.
#' plan(future.batchtools::batchtools_lsf, resources = list(
#'   W = "00:10:00", M = "400",
#'   asis = c("-n 4", "-R 'span[hosts=1]'", "-q freecycle"),
#'   modules = c("r", "jags"),
#'   details = TRUE
#' ))
#'
#' f <- future({
#'   data.frame(
#'     hostname = Sys.info()[["nodename"]],
#'           os = Sys.info()[["sysname"]],
#'        cores = unname(parallelly::availableCores()),
#'      modules = Sys.getenv("LOADEDMODULES")
#'   )
#' })
#' info <- value(f)
#' print(info)
#' 
#' @references
#' * <https://en.wikipedia.org/wiki/IBM_Spectrum_LSF>
#'
#' @export
batchtools_lsf <- function(..., template = "lsf", scheduler.latency = 1.0, fs.latency = 65.0, resources = list(), delete = getOption("future.batchtools.delete", "on-success"), workers = getOption("future.batchtools.workers", default = 100L)) {
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
