#' @export
BatchtoolsSlurmFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "slurm")
  core[["futureClasses"]] <- c("BatchtoolsSlurmFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsSlurmFutureBackend", class(core)))
  core
}


#' A batchtools slurm backend resolves futures in parallel via a Slurm job scheduler
#'
#' @inheritParams BatchtoolsTemplateFutureBackend
#'
#' @param template (optional) Name of job-script template to be searched
#' for by [batchtools::findTemplateFile()]. If not found, it defaults to
#' the `templates/slurm.tmpl` part of this package (see below).
#'
#' @param \ldots Not used.
#'
#' @details
#' Batchtools slurm futures use \pkg{batchtools} cluster functions
#' created by [batchtools::makeClusterFunctionsSlurm()], which requires
#' that Slurm commands `sbatch`, `squeue`, and `scancel` are installed on
#' the current machine.
#'
#' The default template script `templates/slurm.tmpl` can be found in:
#'
#' ```r
#' system.file("templates", "slurm.tmpl", package = "future.batchtools")
#' ```
#'
#' and comprise:
#'
#' `r paste(c("\x60\x60\x60bash", readLines("inst/templates/slurm.tmpl"), "\x60\x60\x60"), collapse = "\n")`
#'
#' This template and the built-in [batchtools::makeClusterFunctionsSlurm()]
#' have been verified to work with Slurm v21.08.4 running on a Rocky 8 Linux
#' cluster with an NFS global filesystem.
#'
#'
#' @examplesIf interactive()
#' library(future)
#'
#' # Limit runtime to 3 minutes and memory to 200 MiB per future
#' plan(future.batchtools::batchtools_slurm, resources = list(time = "00:03:00", mem = "200M"))
#'
#' f <- future(Sys.info())
#' info <- value(f)
#' print(info)
#'
#' @references
#' * <https://en.wikipedia.org/wiki/Slurm_Workload_Manager>
#'
#' @export
batchtools_slurm <- function(..., template = "slurm", scheduler.latency = 1.0, fs.latency = 65.0, resources = list(), workers = getOption("future.batchtools.workers", default = 100L)) {
 stop("INTERNAL ERROR: The future.batchtools::batchtools_slurm() must never be called directly")
}
class(batchtools_slurm) <- c(
  "batchtools_slurm", "batchtools_template",
  "batchtools_multiprocess", "batchtools",
  "multiprocess", "future", "function"
)
attr(batchtools_slurm, "tweakable") <- c(
  "workers",
  "finalize",
  ## Arguments to batchtools::makeClusterFunctionsSlurm()
  "array.jobs", "nodename", "scheduler.latency", "fs.latency"
)
attr(batchtools_slurm, "init") <- TRUE
attr(batchtools_slurm, "factory") <- BatchtoolsSlurmFutureBackend
