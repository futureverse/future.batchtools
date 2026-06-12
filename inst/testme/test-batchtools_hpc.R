#' @tags skip_on_cran
#' 
#' # On an SGE cluster:
#' R_FUTURE_TESTS_STRATEGIES=batchtools_sge NOT_CRAN=true tests/test-batchtools_hpc.R
#'
#' # On a Slurm cluster:
#' R_FUTURE_TESTS_STRATEGIES=batchtools_slurm NOT_CRAN=true tests/test-batchtools_hpc.R

library(future)
library(future.batchtools)

## Setup all strategies including custom once for testing on HPC environments
print(all_strategies())

message("All HPC strategies:")

strategies <- c("batchtools_lsf", "batchtools_openlava", "batchtools_sge",
                "batchtools_slurm", "batchtools_torque")
mprint(strategies, debug = TRUE)

message("Supported HPC strategies:")
strategies <- strategies[sapply(strategies, FUN = test_strategy)]
mprint(strategies, debug = TRUE)

for (strategy in strategies) {
  message("Resource specifications:")
  resources <- list(
    details = TRUE
  )
  if (strategy == "batchtools_sge") {
    resources[["h_rt"]] <- "00:02:00"
    resources[["mem_free"]] <- "100M"
  } else if (strategy == "batchtools_slurm") {
    resources[["time"]] <- "00:02:00"
    resources[["mem"]] <- "100M"
  }
  str(resources)
  
  plan(strategy, resources = resources)
  print(plan())

  f <- future(42L)
  print(f)
  v <- value(f)
  print(v)
  stopifnot(v == 42L)

  message(sprintf("*** %s() ... DONE", strategy))
}

