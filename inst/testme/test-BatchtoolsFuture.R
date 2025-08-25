library(future)

message("*** BatchtoolsFuture() ...")

message("*** BatchtoolsFuture() - cleanup ...")

plan(future.batchtools::batchtools_local)

f <- future({ 1L })
res <- await(f, cleanup = TRUE)
print(res)
stopifnot(res$value == 1L)

message("*** BatchtoolsFuture() - cleanup ... DONE")


message("*** BatchtoolsFuture() - deleting exceptions ...")

## Printing a deleted future
f <- future(42L)
print(f)
v <- value(f)
print(v)
stopifnot(v == 42L)
res <- delete(f)
print(f)
res <- delete(f)
print(f)

message("*** BatchtoolsFuture() - deleting exceptions ... DONE")


message("*** BatchtoolsFuture() - registry exceptions ...")

## Non-existing batchtools registry
f <- future({ x <- 1 })

## Hack to emulate where batchtools registry is deleted or fails
f$state <- "running"
if (!is.null(f$config$reg)) {
  path <- f$config$reg$file.dir
  unlink(path, recursive = TRUE)
}

res <- tryCatch({
  value(f)
}, error = function(ex) ex)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  await(f)
}, error = function(ex) ex)
print(res)
stopifnot(inherits(res, "error"))


message("*** BatchtoolsFuture() - registry exceptions ... DONE")


message("*** BatchtoolsFuture() - timeout ...")

if (fullTest && availableCores(constraints = "multicore") > 1) {
  plan(future.batchtools::batchtools_multicore)

  options(future.wait.timeout = 0.15, future.wait.interval = 0.1)

  f <- future({
    Sys.sleep(5)
    x <- 1
  })

  res <- tryCatch({
    value(f)
  }, error = function(ex) ex)
  stopifnot(inherits(res, "error"))
}


message("*** BatchtoolsFuture() - timeout ... DONE")


message("*** BatchtoolsFuture() ... DONE")
