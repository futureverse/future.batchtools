#' @tags listFutures
#' @tags skip_on_cran

library(future)
library(future.batchtools)

message("*** listFutures() ...")

## batchtools::makeClusterFunctionsSocket() requires the 'snow' package
if (requireNamespace("snow", quietly = TRUE)) {
  ## Use batchtools' socket cluster functions, which run jobs in parallel
  ## without requiring an HPC scheduler
  cf <- batchtools::makeClusterFunctionsSocket(2L)
  plan(batchtools_custom, cluster.functions = cf, workers = 2L)

  backend <- plan("backend")

  futures <- listFutures(backend)
  print(futures)
  stopifnot(nrow(futures) == 0L)

  ## An active future should be listed
  f <- future({ Sys.sleep(3); 42 })
  futures <- listFutures(backend)
  print(futures)
  stopifnot(nrow(futures) == 1L)

  v <- value(f)
  stopifnot(identical(v, 42))

  ## A collected future should no longer be listed
  futures <- listFutures(backend)
  print(futures)
  stopifnot(nrow(futures) == 0L)

  plan(sequential)
} else {
  message("Skipping test: package 'snow' is not installed")
}

message("*** listFutures() ... DONE")
