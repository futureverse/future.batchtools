#' @export
BatchtoolsSGEFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "sge")
  core[["futureClasses"]] <- c("BatchtoolsSGEFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsSGEFutureBackend", class(core)))
  core
}

#' A batchtools SGE backend resolves futures in parallel via a SGE job scheduler
#'
#' @inheritParams BatchtoolsFutureBackend
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
#' @examplesIf interactive()
#' # Limit runtime to 3 minutes and memory to 200 MiB per future
#' plan(batchtools_sge, resources = list(h_rt = "00:03:00", mem_free = "200M"))
#'
#' message("Main process ID: ", Sys.getpid())
#'
#' f <- future(Sys.getpid())
#' pid <- value(f)
#' message("Worker process ID: ", pid)
#' 
#' @references
#' * <https://en.wikipedia.org/wiki/Oracle_Grid_Engine>
#'
#' @export
batchtools_sge <- function(..., template = "sge", scheduler.latency = 1.0, fs.latency = 65.0) {
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
