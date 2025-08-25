#' @export
BatchtoolsOpenLavaFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "openlava")
  core[["futureClasses"]] <- c("BatchtoolsOpenLavaFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsOpenLavaFutureBackend", class(core)))
  core
}


#' A batchtools openlava backend resolves futures in parallel via a OpenLava job scheduler
#'
#' @inheritParams BatchtoolsTemplateFutureBackend
#'
#' @param template (optional) Name of job-script template to be searched
#' for by [batchtools::findTemplateFile()]. If not found, it defaults to
#' the `templates/openlava.tmpl` part of this package (see below).
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools OpenLava futures use \pkg{batchtools} cluster functions
#' created by [batchtools::makeClusterFunctionsOpenLava()], which are used
#' to interact with the OpenLava job scheduler. This requires that OpenLava
#' commands `bsub`, `bjobs`, and `bkill` are available on the current
#' machine.
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
#' library(future)
#'
#' # Limit runtime to 10 minutes and total memory to 400 MiB per future,
#' # request a parallel environment with four slots on a single host.
#' # Submit to the 'freecycle' queue. Load environment modules 'r' and
#' # 'jags'. Report on job details at startup and at the end of the job.
#' plan(future.batchtools::batchtools_openlava, resources = list(
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
#' * <https://en.wikipedia.org/wiki/OpenLava>
#'
#' @export
batchtools_openlava <- function(..., template = "openlava", scheduler.latency = 1.0, fs.latency = 65.0, resources = list(), delete = getOption("future.batchtools.delete", "on-success"), workers = getOption("future.batchtools.workers", default = 100L)) {
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
