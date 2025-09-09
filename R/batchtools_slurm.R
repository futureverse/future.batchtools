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
#' created by [batchtools::makeClusterFunctionsSlurm()], which are used
#' to interact with the Slurm job scheduler. This requires that Slurm
#' commands `sbatch`, `squeue`, and `scancel` are available on the current
#' machine.
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
#' have been verified to work on a few different Slurm HPC clusters;
#'
#'  1. Slurm 21.08.4, Rocky Linux 8, NFS global filesystem (August 2025)
#'  2. Slurm 22.05.11, Rocky Linux 8, NFS global filesystem (August 2025)
#'  3. Slurm 23.02.6, Ubuntu 24.04 LTS, NFS global filesystem (August 2025)
#'  4. Slurm 24.11.3, AlmaLinux 9, Lustre global filesystem (September 2025)
#'
#'
#' @section Known issue "Error Future of class BatchtoolsSlurmFuture expired":
#'
#' Some users report (e.g. Issue [#74](https://github.com/futureverse/future.batchtools/issues/74), Discussion [#810](https://github.com/futureverse/future/discussions/810))
#' that they fail to launch futures on their Slurm clusters using
#' `plan(batchtools_slurm)`. When attempted, these futures fail with an
#' error on the "future being expired". Specifically, the error received is:
#' 
#' ```
#' Error: Future (<unnamed-1>) of class BatchtoolsSlurmFuture expired, which indicates
#' that it crashed or was killed. No log output file exist (at the moment)
#' In addition: Warning messages:
#' 1: batchtools::waitForJobs(..., timeout = 2592000) returned FALSE
#' 2: In delete.BatchtoolsFuture(future) :
#' Will not remove batchtools registry, because the status of the batchtools was
#' 'error', 'defined', 'expired', 'submitted'
#' ```
#'
#' We suspect this is related to how some Slurm schedulers provision jobs.
#' But, because I cannot reproduce this on any of the three Slurm clusters
#' I have access to, it is hard for me to troubleshoot and fix this myself.
#' So, I need your help to figure this one out. If you get this error, please
#' get in touch, preferably by opening an issue at
#' <https://github.com/futureverse/future.batchtools/issues> and share what
#' version of **future.batchtools** you have installed and what version of
#' Slurm (`sbatch --version`) your system have. Then we take it from there.
#' Thank you.
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


# Patch Slurm cluster functions listJobsQueued() and listJobsRunning()
# to use `sacct` instead of `squeue`
#' @importFrom batchtools assertRegistry runOSCommand
#' @importFrom utils tail
patchClusterFunctionsSlurm <- function(cf) {
  OSError <- import_from("OSError", package = "batchtools")
  stopifnot(inherits(cf, "ClusterFunctions"))

  env <- environment(cf[["listJobsQueued"]])
  array.jobs <- env[["array.jobs"]]
  getClusters <- env[["getClusters"]]
  nodename <- env[["nodename"]]
  
  listJobs <- function(reg, args) {      
    assertRegistry(reg, writeable = FALSE)
    args <- c(args, "--user=$USER", "--noheader", "--parsable2", "--allocations", "--format=JobID")
    clusters <- getClusters(reg)
    if (length(clusters) > 0) {
       args <- c(args, sprintf("--clusters=%s", clusters))
    }
    res <- runOSCommand("sacct", args, nodename = nodename)
    if (res$exit.code > 0L) {
      OSError("Listing of jobs failed", res)
    }
    if (length(clusters) > 0) {
      res <- tail(res$output, -1L)
    } else {
      res <- res$output
    }
    res
  } ## listJobs()
  
  cf$listJobsQueued <- function(reg) {
    ## List PENDING (PD) and REQUEUED (RQ) jobs
    listJobs(reg, "--state=PD,RQ")
  }                                                          
  
  cf$listJobsRunning <- function(reg) {
    ## List RUNNING (R), SUSPENDED (S), RESIZING (RS) jobs
    listJobs(reg, "--state=R,S,RS")
  }

  cf
} ## patchClusterFunctionsSlurm()



# Patch Slurm cluster functions listJobsQueued() and listJobsRunning()
# to use `sacct` instead of `squeue`
#' @importFrom batchtools assertRegistry runOSCommand
#' @importFrom utils tail
patchClusterFunctionsSlurm2 <- function(cf) {
  OSError <- import_from("OSError", package = "batchtools")
  stopifnot(inherits(cf, "ClusterFunctions"))

  env <- environment(cf[["listJobsQueued"]])
  array.jobs <- env[["array.jobs"]]
  getClusters <- env[["getClusters"]]
  nodename <- env[["nodename"]]
  org_listJobsQueued <- env[["listJobsQueued"]]

  isJobQueued <- function(reg, batch_id, since = NULL) {
    stopifnot(length(batch_id) == 1L, !is.na(batch_id), nzchar(batch_id))
    stopifnot(is.null(since) || inherits(since, "POSIXct"))
    
    ## FIXME: Add also --starttime=<start time>, because 'sacct' only returns jobs ran today
    args <- c("--user=$USER", "--noheader", "--parsable2", "--allocations", "--format=State", sprintf("--jobs=%s", batch_id), sprintf("--start-time=%s", format(since - 15*60, format = "%FT%T"))) ## Allow for 15-min time offsets
    clusters <- getClusters(reg)
    if (length(clusters) > 0) {
       args <- c(args, sprintf("--clusters=%s", clusters))
    }
    res <- runOSCommand("sacct", args, nodename = nodename)
    if (res$exit.code > 0L) {
      OSError("Failed to check if job is pending", res)
    }
    if (length(clusters) > 0) {
      res <- tail(res$output, -1L)
    } else {
      res <- res$output
    }

    if (length(res) == 0) return(FALSE)
    
    res %in% c("PENDING", "REQUEUED")
  } ## isJobQueued()
  
  cf$listJobsQueued <- function(reg) {
    batch_id <- getOption("future.batchtools.batch_id", NULL)
    
    ## Queued jobs according to 'squeue'
    jobs <- org_listJobsQueued(reg)
    if (is.null(batch_id)) return(jobs)

    ## Is the job queued?
    if (length(jobs) > 0) {
      jobs <- intersect(jobs, as.character(batch_id))
      if (length(jobs) > 0) return(jobs)
    }

    ## Ask 'sacct' it if is PENDING or REQUEUED
    submitted_on <- getOption("future.batchtools.submitted_on", NULL)
    if (isJobQueued(reg, batch_id, since = submitted_on)) {
      jobs <- as.character(batch_id)
    }

    jobs
  }

  cf
} ## patchClusterFunctionsSlurm2()


#' @importFrom batchtools makeClusterFunctionsSlurm

makeClusterFunctionsSlurm2 <- function(template = "slurm", array.jobs = TRUE, nodename = "localhost", scheduler.latency = 1, fs.latency = 65, ...) {
  cf <- makeClusterFunctionsSlurm(template = template, array.jobs = array.jobs, nodename = nodename, scheduler.latency = scheduler.latency, fs.latency =fs.latency, ...)
  cf <- patchClusterFunctionsSlurm2(cf)
  cf
}
