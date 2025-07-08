library(future.batchtools)

message("*** batchtools_template() ...")

## NOTE: Here we use invalid 'workers = FALSE' in order to
## prevent the batchtools future from actually starting,
## because we cannot assume that system has these schedulers.
## NOTE: Some of them will give an earlier error because
## no default template file was found.
res <- try(plan(batchtools_lsf, workers = FALSE))
stopifnot(inherits(res, "try-error"))

res <- try(plan(batchtools_openlava, workers = FALSE))
stopifnot(inherits(res, "try-error"))

res <- try(plan(batchtools_sge, workers = FALSE))
stopifnot(inherits(res, "try-error"))

res <- try(plan(batchtools_slurm, workers = FALSE))
stopifnot(inherits(res, "try-error"))

res <- try(plan(batchtools_torque, workers = FALSE))
stopifnot(inherits(res, "try-error"))

message("*** batchtools_template() ... DONE")

