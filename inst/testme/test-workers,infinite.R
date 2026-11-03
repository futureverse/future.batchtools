#' @tags workers

library(future)
library(future.batchtools)

message("*** workers = Inf ...")

## Use batchtools' socket cluster functions, which run jobs in parallel
## without requiring an HPC scheduler
cf <- batchtools::makeClusterFunctionsSocket(2L)
plan(batchtools_custom, cluster.functions = cf, workers = Inf)

stopifnot(is.infinite(nbrOfWorkers()))

fs <- lapply(1:3, FUN = function(x) future(x))
vs <- value(fs)
stopifnot(identical(unlist(vs), 1:3))

message("*** workers = Inf ... done")
