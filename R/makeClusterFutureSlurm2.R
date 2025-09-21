# Patch Slurm cluster functions listJobsQueued() and listJobsRunning()
# to use `sacct` instead of `squeue`
#' @importFrom batchtools assertRegistry
#' @importFrom utils tail
patchClusterFunctionsSlurm2 <- function(cf) {
  OSError <- import_from("OSError", package = "batchtools")
  stopifnot(inherits(cf, "ClusterFunctions"))

  env <- environment(cf[["listJobsQueued"]])
  array.jobs <- env[["array.jobs"]]
  getClusters <- env[["getClusters"]]
  nodename <- env[["nodename"]]
  org_listJobsQueued <- env[["listJobsQueued"]]

  ## Patch submitJob() to use runOSCommand(..., stderr = NA),
  ## which captures 'stderr' separately from 'stdout'
  ## See https://github.com/mlr-org/batchtools/pull/314
  submitJob <- cf[["submitJob"]]
  env_submitJob <- new.env(parent = environment(submitJob))
  env_submitJob[["runOSCommand"]] <- function(..., stderr = NA) {
    debug <- isTRUE(getOption("future.batchtools.debug"))
    if (debug) {
      mdebugf_push("runOSCommand(..., stderr = %s) ...", stderr)
      mprint(list(args = list(..., stderr = stderr)))
      on.exit(mdebugf_pop())
    }
    runOSCommand(..., stderr = stderr)
  }
  environment(submitJob) <- env_submitJob
  cf[["submitJob"]] <- submitJob

  ## Allow for a 15-minute offset in time between host and Slurm's sacct server
  isJobQueued <- function(reg, batch_id, since = NULL, offset = 15*60) {
    stopifnot(length(batch_id) == 1L, !is.na(batch_id), nzchar(batch_id))
    stopifnot(is.null(since) || inherits(since, "POSIXct"))
    
    args <- c("--user=$USER", "--noheader", "--parsable2", "--allocations", "--format=State", sprintf("--jobs=%s", batch_id), sprintf("--starttime=%s", format(since - offset, format = "%FT%T")))
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
    debug <- isTRUE(getOption("future.batchtools.debug"))
    if (debug) {
      mdebugf_push("[makeClusterFunctionsSlurm2()]$listJobsQueued() ...")
      on.exit(mdebugf_pop())
    }
    
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




#' ClusterFunctions for Slurm Systems (patched)
#'
#' This functions enhances [batchtools::makeClusterFunctionsSlurm()] with
#' a few patches.
#' Firstly, it patches the `listJobsQueued()` cluster function such that it
#' falls back to querying Slurm's account database (`sacct`), if the future
#' was _not_ found in the Slurm job queue (`squeue`), which might be the case
#' when Slurm provisions a job that was just submitted to the scheduler.
#' Secondly, it patched the `submitJob()` cluster function such that the
#' system call to `sbatch` does to capture stderr together with stdout, but
#' rather separately such that any extra INFO messages from `sbatch` do
#' not corrupt the output intended to come from stdout only.
#' 
#' @inheritParams batchtools::makeClusterFunctionsSlurm
#'
#' @return
#' A [batchtools::ClusterFunctions] object.
#'
#' @importFrom batchtools makeClusterFunctionsSlurm
#' @export
makeClusterFunctionsSlurm2 <- function(template = "slurm", array.jobs = TRUE, nodename = "localhost", scheduler.latency = 1, fs.latency = 65) {
  cf <- makeClusterFunctionsSlurm(template = template, array.jobs = array.jobs, nodename = nodename, scheduler.latency = scheduler.latency, fs.latency =fs.latency)
  cf <- patchClusterFunctionsSlurm2(cf)
  cf
}
