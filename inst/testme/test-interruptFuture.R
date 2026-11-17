library(future)
library(batchtools)

message("*** interruptFuture() ...")

message("- killJobs() finds nothing to kill")

## Cluster functions for a scheduler that accepts jobs, but never lists
## them as queued or running, e.g. a job that was just submitted or is
## held. For such jobs, batchtools::killJobs() returns an empty table
## without attempting to kill anything
killed <- character(0L)
cf <- makeClusterFunctions(
  name = "Unlisted",
  submitJob = function(reg, jc) {
    makeSubmitJobResult(status = 0L, batch.id = "unlisted-1")
  },
  killJob = function(reg, batch.id) {
    killed <<- c(killed, batch.id)
    invisible(TRUE)
  },
  listJobsQueued = function(reg) character(0L),
  listJobsRunning = function(reg) character(0L)
)

plan(future.batchtools::batchtools_custom, cluster.functions = cf)

f <- future(42L)
stopifnot(inherits(f, "BatchtoolsFuture"))
state0 <- f[["state"]]
print(state0)
stopifnot(state0 != "interrupted")

backend <- f[["backend"]]
f <- interruptFuture(backend, future = f)

## Nothing was killed ...
stopifnot(length(killed) == 0L)

## ... so the state must not change to 'interrupted'
print(f[["state"]])
stopifnot(f[["state"]] == state0)

## Cleanup
plan(sequential)
## The fake job never runs, so remove its registry folder manually
unlink(f[["config"]][["reg"]][["file.dir"]], recursive = TRUE)
rm(list = "f")
gc()

message("*** interruptFuture() ... DONE")
