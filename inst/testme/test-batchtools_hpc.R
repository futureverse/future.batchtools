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
  plan(strategy)
  print(plan())

  f <- future(42L)
  print(f)
  v <- value(f)
  print(v)
  stopifnot(v == 42L)

  x %<-% Sys.info()
  print(x)

  message(sprintf("*** %s() ... DONE", strategy))
}

