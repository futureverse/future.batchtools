#' @export
BatchtoolsSGEFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "sge")
  core[["futureClasses"]] <- c("BatchtoolsSGEFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsSGEFutureBackend", class(core)))
  core
}

#' A batchtools SGE backend resolves futures in parallel via a SGE job scheduler
#'
#' @inheritParams BatchtoolsTemplateFutureBackend
#'
#' @param template (optional) Name of job-script template to be searched
#' for by [batchtools::findTemplateFile()]. If not found, it defaults to
#' the `templates/sge.tmpl` part of this package (see below).
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools sge futures use \pkg{batchtools} cluster functions
#' created by [batchtools::makeClusterFunctionsSGE()], which requires
#' that SGE commands `qsub`, `qstat`, and `qdel` are installed on
#' the current machine.
#'
#' The default template script `templates/sge.tmpl` can be found in:
#'
#' ```r
#' system.file("templates", "sge.tmpl", package = "future.batchtools")
#' ```
#'
#' and comprise:
#'
#' `r paste(c("\x60\x60\x60bash", readLines("inst/templates/sge.tmpl"), "\x60\x60\x60"), collapse = "\n")`
#'
#' This template and the built-in [batchtools::makeClusterFunctionsSGE()]
#' have been verified to work with SGE 8.1.9 (Son of Grid Engine) running on
#' a Rocky 8 Linux cluster with a BeeGFS global filesystem.
#'
#'
#' @examplesIf interactive()
#' library(future)
#'
#' # Limit runtime to 10 minutes and memory to 400 MiB per future,
#' # request a parallel environment with four slots on a single host.
#' # Submit to the 'freecycle' queue.
#' plan(future.batchtools::batchtools_sge, resources = list(
#'   h_rt = "00:10:00", mem_free = "100M",  ## memory is per process
#'   asis = c("-pe smp 4", "-q freecycle.q")
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
#' * <https://en.wikipedia.org/wiki/Oracle_Grid_Engine>
#'
#' @export
batchtools_sge <- function(..., template = "sge", scheduler.latency = 1.0, fs.latency = 65.0, resources = list(), delete = getOption("future.batchtools.delete", "on-success"), workers = getOption("future.batchtools.workers", default = 100L)) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_sge() must never be called directly")
}
class(batchtools_sge) <- c(
  "batchtools_sge", "batchtools_template",
  "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_sge, "tweakable") <- c(
  "workers",
  "finalize",
  ## Arguments to batchtools::makeClusterFunctionsSGE()
  "nodename", "scheduler.latency", "fs.latency"
)
attr(batchtools_sge, "init") <- TRUE
attr(batchtools_sge, "factory") <- BatchtoolsSGEFutureBackend
