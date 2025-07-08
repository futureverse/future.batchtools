library(future.batchtools)
library(listenv)

message("*** nbrOfWorkers() ...")

message("*** nbrOfWorkers() - local, interactive ...")

n <- nbrOfWorkers(batchtools_local)
message("Number of workers: ", n)
stopifnot(n == 1L)

n <- nbrOfFreeWorkers(batchtools_local)
message("Number of free workers: ", n)
stopifnot(n == 1L)

n <- nbrOfFreeWorkers(batchtools_local, background = TRUE)
message("Number of free background workers: ", n)
stopifnot(n == 0L)

n <- nbrOfWorkers(batchtools_interactive)
message("Number of workers: ", n)
stopifnot(n == 1L)

n <- nbrOfFreeWorkers(batchtools_interactive)
message("Number of free workers: ", n)
stopifnot(n == 1L)

n <- nbrOfFreeWorkers(batchtools_interactive, background = TRUE)
message("Number of free background workers: ", n)
stopifnot(n == 0L)


plan(batchtools_local)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 1L)

n <- nbrOfFreeWorkers()
message("Number of free workers: ", n)
stopifnot(n == 1L)

n <- nbrOfFreeWorkers(background = TRUE)
message("Number of free background workers: ", n)
stopifnot(n == 0L)

plan(batchtools_interactive)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 1L)

n <- nbrOfFreeWorkers()
message("Number of free workers: ", n)
stopifnot(n == 1L)

n <- nbrOfFreeWorkers(background = TRUE)
message("Number of free background workers: ", n)
stopifnot(n == 0L)


message("*** nbrOfWorkers() - local, interactive ... DONE")

ncores <- availableCores("multicore")
if (ncores >= 2L) {
message("*** nbrOfWorkers() - multicore ...")

n <- nbrOfWorkers(batchtools_multicore)
message("Number of workers: ", n)
stopifnot(n == ncores)

n <- nbrOfFreeWorkers(batchtools_multicore)
message("Number of free workers: ", n)
stopifnot(n == ncores)

n <- nbrOfFreeWorkers(batchtools_multicore, background = TRUE)
message("Number of free background workers: ", n)
stopifnot(n == ncores)

plan(batchtools_multicore, workers = 2L)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 2L)

n <- nbrOfFreeWorkers()
message("Number of free workers: ", n)
stopifnot(n == 2L)

n <- nbrOfFreeWorkers(background = TRUE)
message("Number of free background workers: ", n)
stopifnot(n == 2L)

workers <- min(2L, ncores)
plan(batchtools_multicore, workers = workers)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == workers)

message("*** nbrOfWorkers() - multicore ... DONE")
} ## if (ncores >= 2L)


message("*** nbrOfWorkers() - templates ...")

## Test with +Inf workers
options(future.batchtools.workers = +Inf)

n <- nbrOfWorkers(batchtools_lsf)
message("Number of workers: ", n)
stopifnot(is.infinite(n))

n <- nbrOfWorkers(batchtools_openlava)
message("Number of workers: ", n)
stopifnot(is.infinite(n))

n <- nbrOfWorkers(batchtools_sge)
message("Number of workers: ", n)
stopifnot(is.infinite(n))

n <- nbrOfWorkers(batchtools_slurm)
message("Number of workers: ", n)
stopifnot(is.infinite(n))

n <- nbrOfWorkers(batchtools_torque)
message("Number of workers: ", n)
stopifnot(is.infinite(n))

message("*** nbrOfWorkers() - templates ... DONE")

message("*** nbrOfWorkers() - custom ...")

cf <- batchtools::makeClusterFunctionsInteractive(external = TRUE)
str(cf)

plan(batchtools_custom, cluster.functions = cf)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 1L)

message("*** nbrOfWorkers() - custom ... DONE")

message("*** nbrOfWorkers() ... DONE")

