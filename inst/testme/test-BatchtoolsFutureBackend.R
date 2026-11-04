library(future.batchtools)

message("*** BatchtoolsFutureBackend() ...")

is_os <- function(name) {
  if (name == "windows") {
    return(.Platform$OS.type == "windows")
  } else {
    grepl(paste0("^", name), R.version$os)
  }
}

# 1. nbrOfWorkers
# Test with default workers
backend <- future.batchtools::BatchtoolsFutureBackend(delete = "never")
n <- future::nbrOfWorkers(backend)
stopifnot(n == 1)

# Test with character workers
backend <- future.batchtools::BatchtoolsFutureBackend(workers = c("n1", "n2"), delete = "never")
n <- future::nbrOfWorkers(backend)
stopifnot(n == 2)

# Test with function workers
backend <- future.batchtools::BatchtoolsFutureBackend(workers = function() 3, delete = "never")
n <- future::nbrOfWorkers(backend)
stopifnot(n == 3)

# 2. nbrOfFreeWorkers
backend <- future.batchtools::BatchtoolsUniprocessFutureBackend(delete = "never")
n <- future.batchtools:::nbrOfFreeWorkers.BatchtoolsUniprocessFutureBackend(backend)
stopifnot(n == 1)
n_bg <- future.batchtools:::nbrOfFreeWorkers.BatchtoolsUniprocessFutureBackend(backend, background = TRUE)
stopifnot(n_bg == 0)

backend <- future.batchtools::BatchtoolsMultiprocessFutureBackend(workers = 2L, delete = "never")
n <- future.batchtools:::nbrOfFreeWorkers.BatchtoolsMultiprocessFutureBackend(backend)
stopifnot(n == 2)

# 3. BatchtoolsUniprocessFutureBackend and BatchtoolsMultiprocessFutureBackend
backend <- future.batchtools::BatchtoolsUniprocessFutureBackend(delete = "never")
stopifnot(inherits(backend, "BatchtoolsUniprocessFutureBackend"))

backend <- future.batchtools::BatchtoolsMultiprocessFutureBackend(workers = 2L, delete = "never")
stopifnot(inherits(backend, "BatchtoolsMultiprocessFutureBackend"))

# 4. More backend factories
backend <- future.batchtools::BatchtoolsLocalFutureBackend(delete = "never")
stopifnot(inherits(backend, "BatchtoolsLocalFutureBackend"))

backend <- future.batchtools::BatchtoolsInteractiveFutureBackend(delete = "never")
stopifnot(inherits(backend, "BatchtoolsInteractiveFutureBackend"))

if (parallelly::supportsMulticore()) {
  backend <- future.batchtools::BatchtoolsMulticoreFutureBackend(workers = 2L, delete = "never")
  stopifnot(inherits(backend, "BatchtoolsMulticoreFutureBackend"))

  ## A single worker falls back to the (synchronous) batchtools local
  ## backend, unless the number of workers is protected using I()
  backend <- future.batchtools::BatchtoolsMulticoreFutureBackend(workers = 1L, delete = "never")
  stopifnot(inherits(backend, "BatchtoolsLocalFutureBackend"))
  stopifnot(!inherits(backend, "BatchtoolsMulticoreFutureBackend"))
  
  backend <- future.batchtools::BatchtoolsMulticoreFutureBackend(workers = I(1L), delete = "never")
  stopifnot(inherits(backend, "BatchtoolsMulticoreFutureBackend"))
}

backend <- future.batchtools::BatchtoolsCustomFutureBackend(cluster.functions = batchtools::makeClusterFunctionsInteractive(external = TRUE), delete = "never")
stopifnot(inherits(backend, "BatchtoolsCustomFutureBackend"))

# 5. HPC backends
b <- future.batchtools::BatchtoolsLsfFutureBackend(template = "lsf", delete = "never")
stopifnot(inherits(b, "BatchtoolsLsfFutureBackend"))
n <- future::nbrOfWorkers(b)
stopifnot(n >= 1)

b <- future.batchtools::BatchtoolsOpenLavaFutureBackend(template = "openlava", delete = "never")
stopifnot(inherits(b, "BatchtoolsOpenLavaFutureBackend"))
n <- future::nbrOfWorkers(b)
stopifnot(n >= 1)

b <- future.batchtools::BatchtoolsSGEFutureBackend(template = "sge", delete = "never")
stopifnot(inherits(b, "BatchtoolsSGEFutureBackend"))
n <- future::nbrOfWorkers(b)
stopifnot(n >= 1)

b <- future.batchtools::BatchtoolsSlurmFutureBackend(template = "slurm", delete = "never")
stopifnot(inherits(b, "BatchtoolsSlurmFutureBackend"))
n <- future::nbrOfWorkers(b)
stopifnot(n >= 1)

b <- future.batchtools::BatchtoolsTorqueFutureBackend(template = "torque", delete = "never")
stopifnot(inherits(b, "BatchtoolsTorqueFutureBackend"))
n <- future::nbrOfWorkers(b)
stopifnot(n >= 1)

# 6. makeClusterFunctionsSlurm2()
cf <- future.batchtools::makeClusterFunctionsSlurm2()
stopifnot(inherits(cf, "ClusterFunctions"))
stopifnot(cf$name == "Slurm")

# 7. batchtools_bash
if (!is_os("windows")) {
  b <- future.batchtools::BatchtoolsBashFutureBackend(template = "bash", delete = "never")
  stopifnot(inherits(b, "BatchtoolsBashFutureBackend"))

  if (nzchar(Sys.which("bash"))) {
    cf <- future.batchtools::makeClusterFunctionsBash()
    stopifnot(cf$name == "Bash")
  }
}

# 8. Argument 'delete' defaults
cf <- batchtools::makeClusterFunctionsInteractive(external = TRUE)
b <- future.batchtools::BatchtoolsFutureBackend(cluster.functions = cf)
stopifnot(identical(b$delete, "on-success"))

oopts <- options(future.batchtools.delete = "never")
b <- future.batchtools::BatchtoolsFutureBackend(cluster.functions = cf)
stopifnot(identical(b$delete, "never"))
options(oopts)

# 9. status(): job ID options passed
observed_batch_id <- NULL
observed_submitted_on <- NULL
cf <- batchtools::makeClusterFunctions("Mock",
  submitJob = function(reg, jc) batchtools::makeSubmitJobResult(0L, "1"),
  listJobsQueued = function(reg) {
    observed_batch_id <<- getOption("future.batchtools.batch_id")
    observed_submitted_on <<- getOption("future.batchtools.submitted_on")
    character(0)
  }
)
reg <- batchtools::makeRegistry(file.dir = tempfile(), make.default = FALSE)
reg[["cluster.functions"]] <- cf
batchtools::batchMap(function(x) x, 1, reg = reg)
reg[["status"]][["batch.id"]] <- "99999"
reg[["status"]][["submitted"]] <- as.numeric(Sys.time())
batchtools::saveRegistry(reg)

t0 <- Sys.time()
f <- structure(new.env(parent = emptyenv()), class = c("BatchtoolsSlurmFuture", "BatchtoolsTemplateFuture", "BatchtoolsFuture", "Future"))
f[["config"]] <- list(reg = reg, jobid = 1L)
f[["submitted_on"]] <- t0
f[["state"]] <- "submitted"

res <- future.batchtools:::status(f)
stopifnot(identical(observed_batch_id, "99999"))
stopifnot(identical(observed_submitted_on, t0))
stopifnot(is.null(getOption("future.batchtools.batch_id")))
stopifnot(is.null(getOption("future.batchtools.submitted_on")))

message("*** BatchtoolsFutureBackend() ... DONE")
