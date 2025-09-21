#' @export
BatchtoolsSlurmFutureBackend <- function(...) {
  core <- BatchtoolsTemplateFutureBackend(..., type = "slurm")
  core[["futureClasses"]] <- c("BatchtoolsSlurmFuture", core[["futureClasses"]])
  core <- structure(core, class = c("BatchtoolsSlurmFutureBackend", class(core)))
  core
}


#' @export
#' @keywords internal
print.BatchtoolsSlurmFutureBackend <- function(x, ...) {  
  NextMethod()
  printf("Slurm version: %s\n", slurm_version())
  invisible(x)
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
#' created by [makeClusterFunctionsSlurm2()], which are used
#' to interact with the Slurm job scheduler. This requires that Slurm
#' commands `sbatch`, `squeue`, `sacct`, and `scancel` are available on
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
#' This template and the built-in [makeClusterFunctionsSlurm2()]
#' have been verified to work on a few different Slurm HPC clusters;
#'
#'  1. Slurm 21.08.4, Rocky Linux 8, NFS global filesystem (September 2025)
#'  2. Slurm 22.05.10, Rocky Linux 9, Lustre global filesystem (September 2025)
#'  3. Slurm 22.05.11, Rocky Linux 8, NFS global filesystem (September 2025)
#'  4. Slurm 23.02.6, Ubuntu 24.04 LTS, NFS global filesystem (September 2025)
#'  5. Slurm 24.11.3, AlmaLinux 9, Lustre global filesystem (September 2025)*
#'
#' (*) Verified with **future.batchtools** 0.20.0, which used
#'     [batchtools::makeClusterFunctionsSlurm()], which the new
#'     [makeClusterFunctionsSlurm2()] enhances.
#'
#'
#' @examplesIf interactive()
#' library(future)
#'
#' # Limit runtime to 10 minutes and memory to 400 MiB per future,
#' # request a parallel environment with four slots on a single host.
#' # Submit to the 'freecycle' partition. Load environment modules 'r' and
#' # 'jags'. Report on job details at startup and at the end of the job.
#' plan(future.batchtools::batchtools_slurm, resources = list(
#'   time = "00:10:00", mem = "400M",
#'   asis = c("--nodes=1", "--ntasks=4", "--partition=freecycle"),
#'   modules = c("r", "jags"),
#'   details = TRUE
#' ))
#'
#' f <- future({
#'   data.frame(
#'      hostname = Sys.info()[["nodename"]],
#'            os = Sys.info()[["sysname"]],
#'     osVersion = utils::osVersion,
#'         cores = unname(parallelly::availableCores())
#'   )
#' })
#' info <- value(f)
#' print(info)
#'
#' # As above, but use R from the Rocker 'r-base' Linux container;
#' #
#' #   mkdir -p ~/lxc
#' #   apptainer build ~/lxc/rocker_r-base.sif docker://rocker/r-base
#' #
#' # Example assumes that 'future.batchtools' has already been installed in
#' # the container to the 'R_LIBS_USER' package folder living on the host;
#' #
#' #   R_LIBS_USER=~/R/rocker-%p-library/%v ~/lxc/rocker_r-base.sif
#' #   ...
#' #   > chooseCRANmirror(ind = 1)
#' #   > install.packages("future.batchtools")
#' #
#' plan(future.batchtools::batchtools_slurm, resources = list(
#'   time = "00:10:00", mem = "400M",
#'   asis = c("--nodes=1", "--ntasks=4", "--partition=freecycle"),
#'   details = TRUE,
#'   envs = c(R_LIBS_USER = "~/R/rocker-%p-library/%v"),
#'   rscript = c("apptainer", "exec", "~/lxc/rocker_r-base.sif", "Rscript")
#' ))
#'
#' f <- future({
#'   data.frame(
#'      hostname = Sys.info()[["nodename"]],
#'            os = Sys.info()[["sysname"]],
#'     osVersion = utils::osVersion,
#'         cores = unname(parallelly::availableCores()),
#'       modules = Sys.getenv("LOADEDMODULES")
#'   )
#' })
#' info <- value(f)
#' print(info)
#'
#' @references
#' * <https://en.wikipedia.org/wiki/Slurm_Workload_Manager>
#'
#' @export
batchtools_slurm <- function(..., template = "slurm", scheduler.latency = 1.0, fs.latency = 65.0, resources = list(), delete = getOption("future.batchtools.delete", "on-success"), workers = getOption("future.batchtools.workers", default = 100L)) {
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


slurm_version <- local({
  version <- NULL
  
  function() {
    if (is.null(version)) {
      out <- tryCatch(system2("scontrol", args = c("--version"), stdout = TRUE, stderr = TRUE), error = identity)
      if (inherits(out, "error")) {
        version <<- "N/A (unexpected output from 'scontrol --version')"
      } else {
        status <- attr(out, "status")
        if (!is.null(status) && status != 0) {
          version <<- "N/A (unexpected output from 'scontrol --version')"
        } else {
          out <- gsub("(^[[:blank:]]+|[[:blank:]]+$)", "", out)
          out <- out[nzchar(out)]
          if (length(out) >= 1) {
            version <<- out[1]
          }
        }
      }
    }
    version
  }
})
