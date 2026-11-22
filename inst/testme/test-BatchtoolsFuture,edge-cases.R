library(future)

message("*** BatchtoolsFuture - edge cases ...")

oopts <- options(
  future.wait.timeout = 0.5,
  future.wait.interval = 0.01,
  future.batchtools.output = FALSE
)

## Fake cluster functions that never run the job. Instead, they mimic a
## scheduler that fails to submit the job, a job that is running forever,
## or a job that disappeared from the scheduler ("expired")
fake_cluster_functions <- function(name, submit_status = 0L, running = TRUE) {
  batchtools::makeClusterFunctions(
    name = name,
    submitJob = function(reg, jc) {
      batchtools::makeSubmitJobResult(status = submit_status, batch.id = "1234", msg = "boom")
    },
    killJob = function(reg, batch.id) TRUE,
    listJobsRunning = function(reg) if (running) "1234" else character(0L),
    scheduler.latency = 0,
    fs.latency = 0
  )
}

## Removes the batchtools registry of a future, if it exists
remove_registry <- function(f) {
  path <- f[["config"]][["reg"]][["file.dir"]]
  if (!is.null(path)) unlink(path, recursive = TRUE)
}

delete <- future.batchtools:::delete
status <- future.batchtools:::status
await <- future.batchtools:::await


message("- BatchtoolsFutureBackend() without a configuration file")
res <- tryCatch({
  future.batchtools::BatchtoolsFutureBackend(conf.file = tempfile())
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))


message("- Future without a batchtools registry")
plan(future.batchtools::batchtools_local)
f <- future(42L)
stopifnot(value(f) == 42L)
f[["config"]] <- NULL
f[[".status"]] <- NULL
print(f)
stopifnot(
  is.na(status(f)),
  is.na(future.batchtools:::finished(f)),
  is.null(future.batchtools:::immediateConditionsPathOfFuture(f)),
  identical(future.batchtools:::getFutureBackendConfigs.BatchtoolsFuture(f), list()),
  isTRUE(delete(f))
)

f2 <- f
conditions <- f2[["conditions"]]
f2[["conditions"]] <- NULL
stopifnot(identical(future.batchtools:::getFutureBackendConfigs.BatchtoolsFuture(f2), list()))
f2[["conditions"]] <- structure(conditions, immediateConditionClasses = character(0L))
stopifnot(identical(future.batchtools:::getFutureBackendConfigs.BatchtoolsFuture(f2), list()))
f2[["conditions"]] <- conditions

## value() launches a lazy future
f <- future(42L, lazy = TRUE)
stopifnot(value(f) == 42L)


message("- A future can only be launched once")
f <- future(42L)
res <- tryCatch(launchFuture(plan("backend"), f), error = identity)
print(res)
stopifnot(inherits(res, "FutureError"))
stopifnot(value(f) == 42L)


message("- Future with an error, and an uncached status")
f <- future(stop("boom"))
res <- tryCatch(value(f), error = identity)
stopifnot(inherits(res, "error"), conditionMessage(res) == "boom")
options(future.batchtools.status.cache = FALSE)
stat <- status(f)
print(stat)
stopifnot("error" %in% stat)
options(future.batchtools.status.cache = TRUE)
print(f)
msg <- future.batchtools:::loggedError(f)
print(msg)
stopifnot(is.character(msg))
remove_registry(f)


message("- Future whose registry has been removed")
f <- future(42L)
remove_registry(f)
f[[".status"]] <- NULL  ## drop cached status
print(f)
stopifnot(
  is.null(f[["result"]]),
  is.na(status(f)),
  !resolved(f),
  is.null(future.batchtools:::immediateConditionsPathOfFuture(f)),
  is.null(future.batchtools:::loggedOutput(f)),
  is.null(future.batchtools:::loggedError(f))
)
res <- tryCatch(result(f), error = identity)
print(res)
stopifnot(inherits(res, "error"))
res <- tryCatch(await(f, cleanup = FALSE), error = identity)
print(res)
stopifnot(inherits(res, "error"))
stopifnot(isTRUE(delete(f)))
res <- tryCatch(delete(f, onMissing = "warning"), warning = identity)
stopifnot(inherits(res, "warning"))
res <- tryCatch(delete(f, onMissing = "error"), error = identity)
stopifnot(inherits(res, "BatchtoolsFutureError"))


message("- Backend argument 'delete' is \"never\"")
f <- future(42L)
resolve(f)
f[["delete"]] <- "never"
stopifnot(value(f) == 42L)
path <- f[["config"]][["reg"]][["file.dir"]]
stopifnot(file_test("-d", path))
stopifnot(identical(delete(f), FALSE))
remove_registry(f)


message("- Job that fails to be submitted")
plan(future.batchtools::batchtools_custom, cluster.functions = fake_cluster_functions("Fail", submit_status = 101L))
f <- future(42L, lazy = TRUE)
res <- tryCatch(value(f), error = identity)
print(res)
stopifnot(
  inherits(res, "FutureLaunchError"),
  grepl("Failed to submit", conditionMessage(res)),
  grepl("boom", conditionMessage(res))
)
remove_registry(f)


message("- Job that never finishes")
plan(future.batchtools::batchtools_custom, cluster.functions = fake_cluster_functions("Never"))
backend <- plan("backend")
f <- future(42L)
stat <- status(f)
print(stat)
stopifnot(!resolved(f), "submitted" %in% stat)
print(f)

res <- tryCatch(suppressWarnings(value(f)), error = identity)
print(res)
stopifnot(inherits(res, "BatchtoolsFutureError"), grepl("AsyncNotReadyError", conditionMessage(res)))

res <- tryCatch(delete(f, onRunning = "error"), error = identity)
stopifnot(inherits(res, "BatchtoolsFutureError"))
res <- tryCatch(delete(f, onRunning = "warning"), warning = identity)
stopifnot(inherits(res, "warning"))
stopifnot(isTRUE(delete(f, onRunning = "skip")))

## A canceled future is resolved, but its result is an interrupt error
state <- f[["state"]]
f[["state"]] <- "canceled"
stopifnot(resolved(f))
res <- suppressWarnings(await(f, cleanup = FALSE))
print(res)
stopifnot(inherits(res, "FutureInterruptError"))
f[["state"]] <- state

## Interrupts disabled
backend[["interrupts"]] <- FALSE
f <- interruptFuture(backend, f)
stopifnot(f[["state"]] != "interrupted")
backend[["interrupts"]] <- TRUE

## Interrupt a job that was never submitted
f2 <- future(42L, lazy = TRUE)
f2[["config"]] <- list(reg = f[["config"]][["reg"]], jobid = NULL)
f2 <- interruptFuture(backend, f2)
stopifnot(f2[["state"]] == "created")

## Interrupt a running job
f <- interruptFuture(backend, f)
stopifnot(f[["state"]] == "interrupted")

## Stop workers, with interrupts disabled
backend[["interrupts"]] <- FALSE
backend <- stopWorkers(backend)
stopifnot(!backend[["interrupts"]])
backend[["interrupts"]] <- TRUE
remove_registry(f)


message("- Job that disappeared from the scheduler (expired)")
plan(future.batchtools::batchtools_custom, cluster.functions = fake_cluster_functions("Expire", running = FALSE))
f <- future(42L)
stat <- status(f)
print(stat)
stopifnot("expired" %in% stat)
res <- tryCatch(suppressWarnings(value(f)), error = identity)
print(res)
stopifnot(inherits(res, "FutureInterruptError"), grepl("expired", conditionMessage(res)))

## The result is already collected
res <- tryCatch(result(f), error = identity)
stopifnot(inherits(res, "FutureInterruptError"))

## The registry is kept, because the job did not succeed
res <- tryCatch(delete(f), warning = identity)
stopifnot(inherits(res, "warning"))
remove_registry(f)


message("- Debug output")
options(future.debug = TRUE, future.batchtools.output = TRUE)
plan(future.batchtools::batchtools_local)
f <- future({ utils::head(1:3, n = 1L) }, label = "debug-test", packages = "utils")
stopifnot(value(f) == 1L)
print(status(f))

## Finalizer with debug output should delete the batchtools registry
## (the backend's own finalizer is disabled, so that it does not do it)
oopts2 <- options(future.finalize = FALSE)
plan(sequential)
plan(future.batchtools::batchtools_local)
stopifnot(!plan("backend")[["finalize"]])
f <- future(42L)
resolve(f)
path <- f[["config"]][["reg"]][["file.dir"]]
stopifnot(file_test("-d", path))
f <- future.batchtools:::add_finalizer(f, debug = TRUE)
rm(list = "f")
gc()
stopifnot(!file_test("-d", path))
options(oopts2)
plan(sequential)

plan(future.batchtools::batchtools_custom, cluster.functions = fake_cluster_functions("Never"))
f <- future(42L)
res <- tryCatch(suppressWarnings(value(f)), error = identity)
stopifnot(inherits(res, "BatchtoolsFutureError"))
backend <- plan("backend")
f <- interruptFuture(backend, f)

backend[["interrupts"]] <- FALSE
backend <- stopWorkers(backend)
backend[["interrupts"]] <- TRUE
remove_registry(f)

options(future.debug = FALSE)

plan(sequential)
options(oopts)

message("*** BatchtoolsFuture - edge cases ... DONE")
