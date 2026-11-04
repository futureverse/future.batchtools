#' @tags tweak registry

library(future)
library(future.batchtools)

message("*** tweak(<batchtools_hpc>, registry = ...) ...")

## The 'registry' argument is used for instance to set the working
## directory of the batchtools registry, which, when testing on HPC
## environments, must be on a file system shared with the compute nodes
registry <- list(work.dir = tempdir())

for (name in c("batchtools_lsf", "batchtools_openlava", "batchtools_sge",
               "batchtools_slurm", "batchtools_torque", "batchtools_custom")) {
  message(sprintf("- %s ...", name))
  strategy <- get(name, mode = "function")
  
  ## No warnings about unknown arguments
  res <- withCallingHandlers({
    tweak(strategy, registry = registry)
  }, warning = function(w) {
    stop("Unexpected warning: ", conditionMessage(w))
  })
  stopifnot(inherits(res, "tweaked"))
  stopifnot("registry" %in% attr(strategy, "tweakable"))
}

message("*** tweak(<batchtools_hpc>, registry = ...) ... done")
