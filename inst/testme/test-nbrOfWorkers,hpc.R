library(future)

message("*** nbrOfWorkers() - HPC backends ...")

oplan <- plan()
oopts <- options(future.batchtools.workers = NULL)

message("- Template backend with default number of workers")
plan(future.batchtools::batchtools_slurm)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 100)
n <- nbrOfFreeWorkers()
message("Number of free workers: ", n)
stopifnot(n == 100)

message("- Template backend with option 'future.batchtools.workers'")
options(future.batchtools.workers = 42L)
plan(sequential)  ## force the batchtools backend to be re-created
plan(future.batchtools::batchtools_slurm)
n <- nbrOfWorkers()
stopifnot(n == 42L)
options(future.batchtools.workers = NULL)

message("- Template backend with 'workers' as a function")
plan(future.batchtools::batchtools_slurm, workers = function() 3L)
n <- nbrOfWorkers()
stopifnot(n == 3L)

message("- Template backend with invalid 'workers'")
for (workers in list(0, Inf, NA_real_, c(2, 3), "a")) {
  res <- tryCatch({
    plan(future.batchtools::batchtools_slurm, workers = workers)
  }, error = identity)
  stopifnot(inherits(res, "error"))
}


message("- Custom backend with 'workers' as a character vector")
cf <- batchtools::makeClusterFunctionsInteractive(external = TRUE)
plan(future.batchtools::batchtools_custom, workers = c("n1", "n2"), cluster.functions = cf)
n <- nbrOfWorkers()
stopifnot(n == 2L)
n <- nbrOfFreeWorkers()
stopifnot(n == 2L)

message("- Custom backend with 'workers' as a function")
plan(future.batchtools::batchtools_custom, workers = function() 2L, cluster.functions = cf)
n <- nbrOfWorkers()
stopifnot(n == 2L)

message("- Custom backend with invalid 'workers'")
res <- tryCatch({
  plan(future.batchtools::batchtools_custom, workers = list(1), cluster.functions = cf)
}, error = identity)
stopifnot(inherits(res, "error"))


message("- nbrOfWorkers() inferred from cluster functions")
plan(sequential)
make_backend <- function(...) {
  structure(list(...), class = c("BatchtoolsFutureBackend", "FutureBackend"))
}

backend <- make_backend(cluster.functions = batchtools::makeClusterFunctionsInteractive())
stopifnot(nbrOfWorkers(backend) == 1L)

backend <- make_backend(workers = c(2, 3))
stopifnot(nbrOfWorkers(backend) == 6)

backend <- make_backend(workers = c("n1", "n2", "n3"))
stopifnot(nbrOfWorkers(backend) == 3L)

backend <- make_backend(workers = function() 4L)
stopifnot(nbrOfWorkers(backend) == 4L)

backend <- make_backend(workers = list(1))
res <- tryCatch(nbrOfWorkers(backend), error = identity)
stopifnot(inherits(res, "error"))

tmpl <- system.file("templates", "slurm.tmpl", package = "future.batchtools")
cf <- batchtools::makeClusterFunctionsSlurm(template = tmpl)
backend <- make_backend(cluster.functions = cf)
stopifnot(nbrOfWorkers(backend) == 100)

message("- nbrOfFreeWorkers() for a generic batchtools backend")
res <- tryCatch(nbrOfFreeWorkers(backend), warning = identity)
stopifnot(inherits(res, "warning"))
n <- suppressWarnings(nbrOfFreeWorkers(backend))
stopifnot(n == 100)

backend <- make_backend(workers = +Inf)
stopifnot(is.infinite(nbrOfFreeWorkers(backend)))

message("- Generic HPC scheduler and option 'future.batchtools.workers'")
backend <- make_backend()
stopifnot(nbrOfWorkers(backend) == 100)

options(future.batchtools.workers = 5.7)
stopifnot(nbrOfWorkers(backend) == 5)

for (value in list(0, NA_real_, "a", c(1, 2))) {
  options(future.batchtools.workers = value)
  res <- tryCatch(nbrOfWorkers(backend), error = identity)
  print(res)
  stopifnot(inherits(res, "error"))
}

options(oopts)
plan(oplan)

message("*** nbrOfWorkers() - HPC backends ... DONE")
