message("*** plan() ...")

message("*** future::plan(future.batchtools::batchtools_local)")
oplan <- future::plan(future.batchtools::batchtools_local)
print(future::plan())
future::plan(oplan)
print(future::plan())

library(future)
library(future.batchtools)

for (type in c("batchtools_interactive", "batchtools_local")) {
  mprintf("*** plan('%s') ...\n", type)

  plan(type)
  stopifnot(inherits(plan("next"), "batchtools"))

  a <- 0
  f <- future({
    b <- 3
    c <- 2
    a * b * c
  })
  a <- 7  ## Make sure globals are frozen
  v <- value(f)
  print(v)
  stopifnot(v == 0)


  ## Customize the 'work.dir' of the batchtools registries
  normalize_path <- function(path) {
    if (!utils::file_test("-d", path)) stop("No such path: ", path)
    opwd <- getwd()
    on.exit(setwd(opwd))
    setwd(normalizePath(path))
    getwd()
  }
  plan(type, registry = list(work.dir = NULL))
  f <- future(42, lazy = TRUE)
  ## In future releases, lazy futures may stay vanilla Future objects
  if (inherits(f, "BatchtoolsFuture")) {
    if (!is.null(f$config$reg)) {
      utils::str(list(
        normalize_path(f$config$reg$work.dir),
        getwd = getwd()
      ))
      stopifnot(normalize_path(f$config$reg$work.dir) == getwd())
    }
  }

  path <- tempdir()
  plan(type, registry = list(work.dir = path))
  f <- future(42, lazy = TRUE)
  ## In future releases, lazy futures may stay vanilla Future objects
  if (inherits(f, "BatchtoolsFuture")) {
    if (!is.null(f$config$reg)) {
      utils::str(list(
        normalize_path(f$config$reg$work.dir),
        path = normalize_path(path)
      ))
      stopifnot(normalize_path(f$config$reg$work.dir) == normalize_path(path))
    }
  }

  mprintf("*** plan('%s') ... DONE\n", type)
} # for (type ...)


message("*** Assert that default backend can be overridden ...")

mpid <- Sys.getpid()
print(mpid)

plan(batchtools_interactive)
pid %<-% { Sys.getpid() }
print(pid)
stopifnot(pid == mpid)

plan(batchtools_local)
pid %<-% { Sys.getpid() }
print(pid)
stopifnot(pid != mpid)


message("*** plan() ... DONE")

