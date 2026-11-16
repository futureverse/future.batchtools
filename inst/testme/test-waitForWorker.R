#' @tags waitForWorker
#' @tags skip_on_cran

library(future)
library(future.batchtools)

message("*** waitForWorker() ...")

## batchtools::makeClusterFunctionsSocket() requires the 'snow' package
if (requireNamespace("snow", quietly = TRUE)) {
  ## Use batchtools' socket cluster functions, which run jobs in parallel
  ## without requiring an HPC scheduler
  cf <- batchtools::makeClusterFunctionsSocket(1L)

  message("- waitForWorker() times out while all workers are occupied")
  ## The backend records 'future.wait.timeout' when the plan is set up ...
  oopts <- options(future.wait.timeout = 0.5)
  plan(batchtools_custom, cluster.functions = cf, workers = 1L)
  ## ... so changing the option afterward should have no effect
  options(future.wait.timeout = 30 * 24 * 60 * 60)

  ## Starts immediately - no worker is occupied yet
  f1 <- future({ Sys.sleep(2); 1 })

  ## Should time out, because there are no free workers
  res <- tryCatch(future(2), error = identity)
  print(res)
  stopifnot(
    inherits(res, "FutureError"),
    grepl("TIMEOUT", conditionMessage(res))
  )

  ## Collect the first future, which frees up the worker
  v1 <- value(f1)
  stopifnot(identical(v1, 1))

  message("- waitForWorker() returns when a worker is available")
  f2 <- future(2)
  v2 <- value(f2)
  stopifnot(identical(v2, 2))

  plan(sequential)
  options(oopts)
} else {
  message("Skipping test: package 'snow' is not installed")
}

message("*** waitForWorker() ... DONE")
