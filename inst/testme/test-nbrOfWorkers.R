library(future)

message("*** nbrOfWorkers() ...")

message("*** nbrOfWorkers() - local, interactive ...")

plan(future.batchtools::batchtools_local)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 1L)

n <- nbrOfFreeWorkers()
message("Number of free workers: ", n)
stopifnot(n == 1L)

n <- nbrOfFreeWorkers(background = TRUE)
message("Number of free background workers: ", n)
stopifnot(n == 0L)

plan(future.batchtools::batchtools_interactive)
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

n <- plan(future.batchtools::batchtools_multicore)

n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == ncores)

n <- nbrOfFreeWorkers()
message("Number of free workers: ", n)
stopifnot(n == ncores)

n <- nbrOfFreeWorkers(background = TRUE)
message("Number of free background workers: ", n)
stopifnot(n == ncores)

plan(future.batchtools::batchtools_multicore, workers = 2L)
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
plan(future.batchtools::batchtools_multicore, workers = workers)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == workers)

message("*** nbrOfWorkers() - multicore ... DONE")
} ## if (ncores >= 2L)

message("*** nbrOfWorkers() - custom ...")

cf <- batchtools::makeClusterFunctionsInteractive(external = TRUE)
str(cf)

## FIXME: Make it possible to *not* set 'workers'
plan(future.batchtools::batchtools_custom, workers = 1L, cluster.functions = cf)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 1L)

message("*** nbrOfWorkers() - custom ... DONE")

message("*** nbrOfWorkers() ... DONE")
